package oxidation
package tool

import parse.Parser
import parse.ast
import java.io.File

import scala.io.Source

object AstDump extends App {

  val inFile = new File(args(0))

  val parser = new Parser

  val res = parser.compilationUnit.parseIterator(Source.fromFile(inFile).getLines().map(_ + "\n"))

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

  def prettyprintDef(d: ast.Def): P = d match {
    case ast.ValDef(name, tpe, value) =>
      s"ValDef($name, $tpe,".nl + prettyprintExp(value).indent + ")"

    case ast.VarDef(name, tpe, value) =>
      s"VarDef($name, $tpe,".nl + prettyprintExp(value).indent + ")"

    case ast.DefDef(name, params, tpe, body) =>
      s"DefDef($name, $params, $tpe,".nl + prettyprintExp(body).indent + ")"

    case ast.StructDef(name, members) =>
      s"StructDef($name, ".nl + members.map {
        case ast.StructMember(name, ast.Type.Named(tpe)) => s"Member($name, $tpe)".p
      }.sep(", ".nl).indent + ")"

  }

  def prettyprintExp(e: ast.Expression): P = e match {
    case ast.IntLit(i) => i.toString

    case ast.BoolLit(b) => b.toString

    case ast.Var(v) => v

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

  res.fold(
    onFailure = (_, _, _) => Console.err.println(res),
    onSuccess = (ast, _) => {
      println("Success!")
      ast.foreach(d => println(stringify(prettyprintDef(d))))
    }
  )

}
