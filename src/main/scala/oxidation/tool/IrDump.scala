package oxidation
package tool

import java.io.{DataOutputStream, File, FileOutputStream, OutputStream, OutputStreamWriter, PrintWriter}

import analyze._
import parse.Parser

import scala.io.Source
import cats._
import cats.data._
import cats.implicits._
import ir.serialization.Serialize
import codegen.pass.Pass
import codegen.{Codegen, pass}
import oxidation.backend.amd64.Amd64BackendPass

object IrDump extends App {

  case class Options(infiles: Seq[File] = Seq.empty,
                     passes: List[Pass] = List.empty,
                     timing: Boolean = false,
                     out: Option[File] = None,
                     format: Format = Textual,
                     dumpAfterEachPass: Boolean = false) extends LogOptions

  sealed trait Format
  case object Textual extends Format
  case object Binary extends Format


  val AllPasses: List[(String, Pass)] = List(
    "explicit-blocks" -> pass.ExplicitBlocks,
    "struct-lowering" -> pass.StructLowering,
    "array-dealiasing" -> pass.ArrayDealiasing,
    "constant-removal" -> pass.ConstantRemoval,
    "expr-weaken" -> pass.ExprWeaken,
    "amd64-backend"   -> Amd64BackendPass
  )

  implicit val passRead: scopt.Read[Pass] =
    scopt.Read.reads(n => AllPasses.find(_._1 == n).map(_._2)
      .getOrElse(throw new IllegalArgumentException(s"No pass named $n")))

  val optParser = new scopt.OptionParser[Options]("astdump") {

    head("irdump", "Compiles a program and prints the resulting intermediate representation")

    opt[Unit]('v', "verbose")
      .action((_, o) => o.copy(timing = true))

    arg[File]("infiles")
      .required().unbounded()
      .action((f, o) => o.copy(infiles = o.infiles :+ f))

    def upTo(p: Pass): List[Pass] = AllPasses.map(_._2).reverse.dropWhile(_ != p).reverse

    opt[Pass]('p', "passupto")
      .action((p, o) => o.copy(passes = upTo(p)))

    opt[File]('o', "outfile")
      .action((f, o) => o.copy(out = Some(f)))

    opt[Unit]('b', "binary")
      .action((_, o) => o.copy(format = Binary))

    opt[Unit]('a', "dump-after-each-pass")
      .action((_, o) => o.copy(dumpAfterEachPass = true))

  }

  def get[L, R](e: Either[L, R]): R = e.fold(l => {
    Console.err.println(l)
    sys.exit(1)
  }, identity)

  optParser.parse(args, Options()).foreach { implicit options =>
    val parser = new Parser
    val parsed = phase("parse") {
      options.infiles.map(f => parser.compilationUnit.parse(Source.fromFile(f).mkString).get.value)
    }
    val symbols = phase("symbol-search") {
      parsed.toVector.foldMap(tlds => get(SymbolSearch.findSymbols(tlds.toVector)))
    }
    val scope = BuiltinSymbols.symbols |+| symbols
    val resolvedSymbols = phase("symbol-resolve") {
      parsed.toVector.flatMap(tlds => get(SymbolResolver.resolveSymbols(tlds.toVector, scope)))
    }

    val typeDefs = resolvedSymbols.collect {
      case d: parse.ast.TypeDef => d
    }
    val ctxt = phase("type-interpret") {
      get(TypeInterpreter.solveTree(typeDefs, Ctxt.default))
    }
    val termDefs = resolvedSymbols.collect {
      case d: parse.ast.TermDef => d
    }
    val deps = phase("dependency-graph") {
      DependencyGraph.build(termDefs).prune
    }
    val typed = phase("type") {
      get(TypeTraverse.solveTree(deps, termDefs, ctxt))
    }

    val irDefs = phase("codegen") {
      typed.map {
        case d: analyze.ast.TermDef => Codegen.compileDef(d)
      }
    }

    def fileAfterPhase(out: File, phase: String): File = {
      val (name, ext) = out.getName.splitAt(out.getName.lastIndexOf('.'))
      new File(out.getParentFile, s"$name-$phase$ext")
    }

    def dumpAfter(phase: String, defs: Set[ir.Def]): Unit = options.out match {
      case None =>
        println(s"--- Ir after phase $phase ---")
        dump(defs, options.format, System.out)
      case Some(f) =>
        val out = fileAfterPhase(f, phase)
        dump(defs, options.format, new FileOutputStream(out))
    }

    if(options.dumpAfterEachPass) {
      dumpAfter("codegen", irDefs)
    }
    val passed = options.passes.foldLeft(irDefs) { (defs, pass) =>
      val res = defs.flatMap(d => pass.extract(pass.txDef(d)))
      if(options.dumpAfterEachPass) {
        dumpAfter(pass.name, res)
      }
      res
    }
    if(!options.dumpAfterEachPass) {
      options.out match {
        case None =>
          dump(passed, options.format, System.out)
        case Some(f) =>
          dump(passed, options.format, new FileOutputStream(f))
      }
    }
  }

  def dump(defs: Set[ir.Def], format: Format, out: OutputStream): Unit = format match {
    case Textual =>
      val writer = new PrintWriter(out, true)
      defs.foreach {
        case ir.Def.Fun(name, params, ret, body, constants) =>
          writer.println(show"def $name(${params.map(_.show).mkString(", ")}): $ret {")
          if(constants.nonEmpty) {
            writer.println("  constants {")
            constants.foreach { e =>
              writer.println(show"    $e")
            }
            writer.println("  }")
          }
          body.foreach { block =>
            writer.println(show"  ${block.name} {")
            block.instructions.foreach { instr =>
              writer.println(show"    $instr")
            }
            writer.println(show"    ${block.flow}")
            writer.println("  }")
          }
          writer.println("}")

        case ir.Def.ExternFun(name, params, ret) =>
          writer.println(show"def $name(${params.map(_.show).mkString(", ")}): $ret = extern")
      }

    case Binary =>
      val s = new Serialize(new DataOutputStream(out))
      s.writeDefs(defs.toSeq)
  }

}
