package oxidation
package tool

import java.io.File

import oxidation.analyze._
import oxidation.parse.Parser

import scala.io.Source

import cats._
import cats.data._
import cats.implicits._
import oxidation.codegen.{ Codegen, ir }

object IrDump extends App {

  case class Options(infiles: Seq[File] = Seq.empty)

  val optParser = new scopt.OptionParser[Options]("astdump") {

    head("irdump", "Compiles a program and prints the resulting intermediate representation")

    arg[File]("infiles")
      .required().unbounded()
      .action((f, o) => o.copy(infiles = o.infiles :+ f))

  }

  def get[L, R](e: Either[L, R]): R = e.fold(l => {
    Console.err.println(l)
    sys.exit(1)
  }, identity)

  optParser.parse(args, Options()).foreach { options =>
    val parser = new Parser
    val parsed = options.infiles.map(f => parser.compilationUnit.parse(Source.fromFile(f).mkString).get.value)
    val symbols = parsed.toVector.foldMap(tlds => get(SymbolSearch.findSymbols(tlds.toVector)))
    val scope = BuiltinSymbols.symbols |+| symbols
    val resolvedSymbols = parsed.toVector.flatMap(tlds => get(SymbolResolver.resolveSymbols(tlds.toVector, scope)))

    val typeDefs = resolvedSymbols.collect {
      case d: parse.ast.TypeDef => d
    }
    val ctxt = get(TypeInterpreter.solveTree(typeDefs, Ctxt.default))
    val termDefs = resolvedSymbols.collect {
      case d: parse.ast.TermDef => d
    }
    val deps = DependencyGraph.build(termDefs).prune
    val typed = get(TypeTraverse.solveTree(deps, termDefs, ctxt))

    val irDefs = typed.map {
      case d: analyze.ast.TermDef => Codegen.compileDef(d)
    }
    irDefs.foreach {
      case ir.Def.Fun(name, params, ret, body) =>
        println(show"def $name(${params.map(_.show).mkString(", ")}): $ret {")
        body.foreach { block =>
          println(show"  ${block.name} {")
          block.instructions.foreach { instr =>
            println(show"    $instr")
          }
          println(show"    ${block.flow}")
          println("  }")
        }
        println("}")
    }
  }

}
