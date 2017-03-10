package oxidation
package tool

import parse.Parser
import parse.ast
import java.io.File

import oxidation.analyze.{BuiltinSymbols, SymbolResolver, SymbolSearch, Symbols}

import scala.io.Source

import cats._
import cats.data._
import cats.implicits._

object AstDump extends App {

  sealed trait P {
    def +(p: P): P = this match {
      case P.Sequence(ps) => P.Sequence(ps :+ p)
      case _ => P.Sequence(Seq(this, p))
    }
    def ++(ps: Iterable[P]): P = this match {
      case P.Sequence(a) => P.Sequence(a ++ ps)
      case _ => P.Sequence(this +: ps.toSeq)
    }

    def nl: P = P.Newline(this)
    def indent: P = P.Indent(this)
    def p: P = this
    def sep(s: P): P = this match {
      case P.Sequence(ps) => P.Sep(s, ps)
      case _ => this
    }
  }
  object P {
    final case class Sequence(ps: Seq[P]) extends P
    final case class Newline(pp: P) extends P
    final case class Indent(pp: P) extends P
    final case class Just(string: String) extends P
    final case class Sep(sep: P, els: Seq[P]) extends P
  }
  implicit def stringToP(string: String): P = P.Just(string)
  implicit def pseqToP(seq: Seq[P]): P = P.Sequence(seq)

  def prettyprintTLD(d: ast.TLD): P = d match {
    case ast.Import(path, names) =>
      val n = names match {
        case ImportSpecifier.All => "All"
        case ImportSpecifier.Members(Seq(one)) => one
        case ImportSpecifier.Members(more) => s"{ ${more mkString ", "} }"
      }
      s"Import(${path mkString "."}, $n)"
    case ast.Module(path) =>
      s"Module(${path mkString "."})"
    case d: ast.Def => prettyprintDef(d)
  }

  def prettyprintDef(d: ast.Def): P = d match {
    case ast.ValDef(name, tpe, value) =>
      s"ValDef($name, ${tpe.map(prettyprintType)},".nl + prettyprintExp(value).indent + ")"

    case ast.VarDef(name, tpe, value) =>
      s"VarDef($name, $tpe,".nl + prettyprintExp(value).indent + ")"

    case ast.DefDef(name, params, tpe, body) =>
      val pars = params match {
        case Some(p) => "Params(".nl + p.map {
            case Param(n, t) => s"Param($n, ".p + prettyprintType(t) + ")"
          }.sep(", ".nl).indent + ")"
        case None => "None".p
      }
      val retType = tpe match {
        case Some(t) => prettyprintType(t)
        case None => "None"
      }
      s"DefDef($name, ".nl + (pars + ",".nl + retType + ",".nl + prettyprintExp(body)).indent + ")"

    case ast.StructDef(name, typeParams, members) =>
      val typeParamList = typeParams match {
        case Some(ps) => s"[${ps mkString ", "}]"
        case None => ""
      }
      s"StructDef($name$typeParamList, ".nl + members.map {
        case StructMember(name, tpe) => s"Member($name, ".p + prettyprintType(tpe) + ")"
      }.sep(", ".nl).indent + ")"

    case ast.TypeAliasDef(name, typeParams, body) =>
      val typeParamList = typeParams match {
        case Some(ps) => s"[${ps mkString ", "}]"
        case None => ""
      }
      s"TypeDef($name$typeParamList, ${prettyprintType(body)})"
  }

  def prettyprintType(t: TypeName): String = t match {
    case TypeName.Named(n) => prettyprintSymbol(n)
    case TypeName.App(t, p) => prettyprintType(t) + "[" + p.map(prettyprintType).mkString(", ") + "]"
  }

  def prettyprintSymbol(s: Symbol): String = s match {
    case Symbol.Local(n) => s"$$$n"
    case Symbol.Global(path) => s"@${path.mkString(".")}"
    case Symbol.Unresolved(n) => s"?$n"
  }

  def prettyprintExp(e: ast.Expression): P = e match {
    case ast.IntLit(i) => i.toString

    case ast.BoolLit(b) => b.toString

    case ast.StringLit(s) => "\"" + s.flatMap {
      case '\n' => "\\n"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c.toString
    } + "\""

    case ast.Var(v) => prettyprintSymbol(v)

    case ast.If(cond, pos, neg) =>
      val prefix = "If(".nl + prettyprintExp(cond).indent + ", ".nl + prettyprintExp(pos).indent
      val els = neg.map(e => ", ".nl + prettyprintExp(e).indent).getOrElse("".p)
      prefix + els + ")"

    case ast.InfixAp(op, left, right) =>
      s"InfixAp($op, ".nl + prettyprintExp(left).indent + ", ".nl + prettyprintExp(right).indent + ")"

    case ast.Block(body) =>
      "Block(".nl + body.map {
        case d: ast.Def => prettyprintDef(d)
        case e: ast.Expression => prettyprintExp(e)
      }.sep(",".nl).indent + ")"

    case ast.App(e, ps) =>
      "App(".nl + (prettyprintExp(e) + ",".nl + "Params(".nl + ps.map {
        expr => prettyprintExp(expr)
      }.sep(",".nl).indent + ")").indent

    case ast.Assign(left, op, right) =>
      s"Assign($op, ".nl + (prettyprintExp(left) + ",".nl + prettyprintExp(right)).indent + ")"

    case ast.PrefixAp(op, right) =>
      s"PrefixApp($op, ".nl + prettyprintExp(right).indent + ")"

    case ast.Select(expr, member) =>
      s"Select($member, ".nl + prettyprintExp(expr).indent + ")"

    case ast.While(cond, body) =>
      s"While(".nl + (prettyprintExp(cond) + ",".nl + prettyprintExp(body)).indent + ")"
  }

  def stringify(p: P): String = {
    val builder = new StringBuilder
    var indent = 0
    def write(p: P): Unit = p match {
      case P.Just(s) =>
        builder ++= s
      case P.Sequence(s) =>
        s.foreach(write)
      case P.Indent(child) =>
        indent += 2
        builder ++= "  "
        write(child)
        indent -= 2
      case P.Newline(child) =>
        write(child)
        builder += '\n'
        builder ++= " " * indent
      case P.Sep(_, Seq()) => ()

      case P.Sep(sep, els) =>
        for(el <- els.init) {
          write(el)
          write(sep)
        }
        write(els.last)
    }
    write(p)
    builder.toString()
  }

  case class Options(infiles: Seq[File] = Seq.empty, resolveSymbols: Boolean = false)

  val optParser = new scopt.OptionParser[Options]("astdump") {

    head("astdump", "Parses an oxidation source file and pretty-prints the resulting ast")

    opt[Unit]('s', "resolveSymbols")
      .action((_, o) => o.copy(resolveSymbols = true))

    arg[File]("infiles")
      .required().unbounded()
      .action((f, o) => o.copy(infiles = o.infiles :+ f))

  }

  val parser = new Parser

  val options = optParser.parse(args, Options()).foreach { options =>
    val res = options.infiles.map { f =>
      val parsed = parser.compilationUnit.parse(Source.fromFile(f).mkString)
      (f, parsed.get.value)
    }
    val res2 = if(options.resolveSymbols) {
      val allSymbols = res.toVector.foldMap {
        case (_, tlds) =>
          SymbolSearch.findSymbols(tlds.toVector).fold({
            case SymbolSearch.DuplicatedSymbolError(s, d) =>
              Console.err.println(s"Symbol $s is already defined!\nin $d")
              sys.exit(1)
          }, identity)
      }
      println("type symbols:")
      allSymbols.types.values.flatten.foreach(s => println(" - " + prettyprintSymbol(s)))
      println("term symbols:")
      allSymbols.terms.values.flatten.foreach(s => println(" - " + prettyprintSymbol(s)))
      val scope = BuiltinSymbols.symbols |+| allSymbols
      res.toVector.traverse {
        case (f, tlds) => SymbolResolver.resolveSymbols(tlds.toVector, scope).map(f -> _)
      }.fold(err => { Console.err.println(err); sys.exit() }, identity)
    } else res
    for((f, defs) <- res2) {
      println(s"${f.getName}:")
      defs.foreach(d => println(stringify(prettyprintTLD(d))))
    }
     /* onFailure = (_, _, _) => Console.err.println(res),
      onSuccess = (ast, _) => {
        ast.foreach(d => println(stringify(prettyprintDef(d))))
      }
    )*/
  }


}
