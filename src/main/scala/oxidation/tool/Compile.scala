package oxidation
package tool

import java.io.{BufferedWriter, File, FileWriter, OutputStreamWriter}

import oxidation.analyze._
import oxidation.parse.Parser

import scala.io.Source
import cats._
import cats.data._
import cats.implicits._
import oxidation.backend.amd64.{Amd64NasmOutput, Amd64Target}
import oxidation.codegen.{Codegen, Name, pass}
import oxidation.codegen.pass.Pass
import oxidation.ir.ConstantPoolEntry
import oxidation.ir.validation.{ValidationError, Validator}

import scala.sys.process.{Process, ProcessLogger}

object Compile {

  case class Options(infiles: Seq[File] = Seq.empty,
                     output: Option[File] = None,
                     validate: Boolean = false,
                     timing: Boolean = false,
                     passthrough: Vector[String] = Vector.empty,
                     withoutConstPropagation: Boolean = false) extends LogOptions

  val optParser = new scopt.OptionParser[Options]("oxc") {

    head("oxc", "Compiles a program and outputs assembly")

    opt[Unit]('t', "timing")
      .action((_, o) => o.copy(timing = true))

    opt[Unit]('v', "validate")
      .action((_, o) => o.copy(validate = true))

    opt[String]('g', "gcc").unbounded()
      .action((s, o) => o.copy(passthrough = o.passthrough :+ s))

    opt[File]('o', "output")
      .action((f, o) => o.copy(output = Some(f)))


    opt[Unit]("no-constant-propagation")
      .action((_, o) => o.copy(withoutConstPropagation = true))

    arg[File]("infiles")
      .required().unbounded()
      .action((f, o) => o.copy(infiles = o.infiles :+ f))

  }

  final case class ParseError(message: String) extends CompileError
  final case class ValidatorError(err: ValidationError, after: String) extends CompileError
  final case class AssemblerError(message: String) extends CompileError
  final case class LinkerError(message: String) extends CompileError
  final case class AnalysisErrors(errors: NonEmptyList[AnalysisError]) extends CompileError

  def get[L, R](e: Either[L, R]): R = e.fold(l => {
    Console.err.println(l)
    sys.exit(1)
  }, identity)

  final case class Stats(spillCount: Int)

  def compile(implicit options: Options): Either[CompileError, (File, Stats)] = {
    for {
      parsed <- options.infiles.toVector.traverse { f =>
        val parser = new Parser(Some(f.getName))
        val res = parser.compilationUnit.parse(Source.fromFile(f).mkString)
        res.fold(
          (_, _, _) => Left(ParseError(res.toString)),
          (r, _) => Right(r)
        )
      }
      symbols <- parsed.traverse(SymbolSearch.findSymbols).map(_.combineAll)
      scope = BuiltinSymbols.symbols |+| symbols
      resolvedSymbols <- parsed.traverse(SymbolResolver.resolveSymbols(_, scope).toValidated).toEither.map(_.flatten).leftMap(AnalysisErrors)
      typeDefs = resolvedSymbols.collect {
        case d: parse.ast.TypeDef => d
      }
      ctxt <- TypeInterpreter.solveTree(typeDefs, Ctxt.default)
      termDefs = resolvedSymbols.collect {
        case d: parse.ast.TermDef => d
      }
      deps = DependencyGraph.build(resolvedSymbols).prune
      typed <- TypeTraverse.solveTree(deps, termDefs, ctxt).leftMap(AnalysisErrors).toEither
      irDefs = typed.map {
        case d: analyze.ast.TermDef => Codegen.compileDef(d)
      }
      _ <-
        if(options.validate)
          irDefs.traverse_(Validator.validateDef).left.map(ValidatorError(_, "codegen"))
        else Right(())
      passes: List[Pass] = List(
        pass.ArrInit,
        pass.ExplicitBlocks,
        pass.DeadBlockRemoval,
        pass.ValInterpretPass,
        pass.ConstantInlining,
        pass.EnumLoweringPass,
        pass.StructLowering,
        pass.UnitRemoval,
        pass.ConstantRemoval,
        pass.ExprWeaken,
        pass.ssa.IntoSSA,
        if(options.withoutConstPropagation) pass.NullPass else pass.ssa.ConstantPropagation,
        pass.ssa.UnusedRegRemoval,
        pass.ssa.FromSSA,
        pass.ArrayDealiasing
      )
      passed <- passes.foldLeft(Right(irDefs.toVector): Either[CompileError, Vector[ir.Def]]) { (defs, pass) =>
        defs.flatMap { defs =>
          val d = pass.extract(pass.txDefs(defs))
          if(options.validate)
            d.traverse_(Validator.validateDef).left.map(ValidatorError(_, s"pass ${pass.name}")).as(d)
          else Right(d)
        }
      }
      constants: Set[ConstantPoolEntry] = passed.flatMap {
        case ir.Def.Fun(_, _, _, _, c) => c
        case _ => Set.empty[ConstantPoolEntry]
      }.toSet
      namedConstants = constants.toVector.zipWithIndex.map {
        case (c, i) => c -> Name.Global(List("$const", i.toString))
      }.toMap

      target = new Amd64Target with Amd64NasmOutput
      baseFile = options.output getOrElse options.infiles.head
      baseName = baseFile.getName.split("\\.").head
      targetFolder = new File(baseFile.getParentFile, "target")
      _ = targetFolder.mkdir()
      asmFile = new File(targetFolder, baseName + ".asm")
      objFile = new File(targetFolder, baseName + ".obj")
      exeFile = options.output.getOrElse(new File(targetFolder, baseName + ".exe"))
      out = new FileWriter(asmFile)
      _ = {
        ( target.outputExtraDefs |+|
          target.outputDefs(passed.toVector)(namedConstants) |+|
          target.outputConstants(namedConstants)
        ).foreach { l =>
          out.write(l)
          out.write('\n')
        }
        out.close()
      }
      _ <- run("nasm", "-fwin64", "-o", objFile.getAbsolutePath, asmFile.getAbsolutePath)
        .left.map(AssemblerError)
      gccOptions = Seq(objFile.getAbsolutePath, show"-Wl,-e${target.EntryPointName}", "-o", exeFile.getAbsolutePath) ++
        options.passthrough.map(s => s"-$s")
      _ <- run(("gcc" +: gccOptions): _*)
        .left.map(LinkerError)
    } yield (exeFile, Stats(target.spillCount))
  }

  private def run(commandline: String*): Either[String, Unit] = {
    val builder = new StringBuilder
    val log = new ProcessLogger {
      override def buffer[T](f: => T) = f

      override def out(s: => String) =
        builder ++= s += '\n'

      override def err(s: => String) =
        builder ++= s += '\n'
    }
    val res = Process(commandline) ! log
    if(res == 0) Right(()) else Left(builder.toString)
  }

  def main(args: Array[String]): Unit = {
    optParser.parse(args, Options()).foreach(opt => get(compile(opt)))
  }


}
