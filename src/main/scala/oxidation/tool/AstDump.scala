package oxidation
package tool

import parse.{IndexTranslator, Parser, Sourcefile, ast}
import java.io.File

import oxidation.analyze._

import scala.io.Source
import cats._
import cats.data._
import cats.implicits._

object AstDump extends App {

  sealed abstract class Stage(val pp: AstPrettyprint)
  case object Parse extends Stage(ParseAstPrettyprint)
  case object Symbols extends Stage(ParseAstPrettyprint)
  case object Types extends Stage(TypedAstPrettyprint)

  case class Options(infiles: Seq[File] = Seq.empty, stage: Stage = Parse)

  val optParser = new scopt.OptionParser[Options]("astdump") {

    head("astdump", "Parses an oxidation source file and pretty-prints the resulting ast")

    opt[Unit]('s', "resolveSymbols")
      .action((_, o) => o.copy(stage = Symbols))

    opt[Unit]('t', "type")
      .action((_, o) => o.copy(stage = Types))

    arg[File]("infiles")
      .required().unbounded()
      .action((f, o) => o.copy(infiles = o.infiles :+ f))

  }

  optParser.parse(args, Options()).foreach { options =>
    val res = options.infiles.map { f =>
      val parser = new Parser(Some(f.getName))
      val parsed = parser.compilationUnit.parse(Source.fromFile(f).mkString)
      (f, parsed.get.value)
    }
    val sources = options.infiles.map { f => f.getName -> Sourcefile(Source.fromFile(f)) }
    implicit val indexTranslator = new IndexTranslator(sources.toMap, None)

    options.stage match {
      case Parse =>
        show(ParseAstPrettyprint)(res)
      case Symbols =>
        show(ParseAstPrettyprint)(resolveSymbols(res))
      case Types =>
        val files = resolveSymbols(res)
        val allDefs = files.flatMap(_._2).toVector
        val allTypeDefs = allDefs.collect {
          case d: parse.ast.TypeDef => d
        }
        val ctxt = TypeInterpreter.solveTree(allTypeDefs.toVector, Ctxt.default)
          .fold(error => {
            Console.err.println(error)
            sys.exit(1)
          }, identity)
        val allTermDefs = files.flatMap(_._2).collect {
          case d: parse.ast.TermDef => d
        }.toVector
        val deps = DependencyGraph.build(allDefs).prune
        val typed = TypeTraverse.solveTree(deps, allTermDefs, ctxt).fold(error => {
          Console.err.println(error)
          sys.exit(1)
        }, identity)
        for(a <- typed) println(TypedAstPrettyprint.stringify(TypedAstPrettyprint.prettyprintDef(a)))
    }
  }


  private def resolveSymbols(res: Seq[(File, Seq[parse.ast.TLD])]): Seq[(File, Seq[parse.ast.TLD])] = {
    val allSymbols = res.toVector.foldMap {
      case (_, tlds) =>
        SymbolSearch.findSymbols(tlds.toVector).fold({
          case SymbolSearch.DuplicatedSymbolError(s, d) =>
            Console.err.println(s"Symbol $s is already defined!\nin $d")
            sys.exit(1)
        }, identity)
    }
    val scope = BuiltinSymbols.symbols |+| allSymbols
    val files = res.toVector.traverse {
      case (f, tlds) => SymbolResolver.resolveSymbols(tlds.toVector, scope).map(f -> _)
    }.fold(err => {
      Console.err.println(err)
      sys.exit()
    }, identity)
    files
  }

  def show(pp: AstPrettyprint)(tlds: Seq[(File, Seq[pp.ast.TLD])])(implicit translator: IndexTranslator): Unit = {
    for((f, defs) <- tlds) {
      println(s"${f.getName}:")
      defs.foreach(d => println(pp.stringify(pp.prettyprintTLD(d))))
    }
  }


}
