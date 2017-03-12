package oxidation
package tool
import oxidation.analyze.Typed

trait AstPrettyprint {

  val ast: Ast

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
      s"ValDef($name, ${tpe.map(prettyprintType)},".nl + prettyprintTypedExp(value).indent + ")"

    case ast.VarDef(name, tpe, value) =>
      s"VarDef($name, $tpe,".nl + prettyprintTypedExp(value).indent + ")"

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
      s"DefDef($name, ".nl + (pars + ",".nl + retType + ",".nl + prettyprintTypedExp(body)).indent + ")"

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

  def prettyprintTypedExp(e: ast.Typed[ast.Expression]): P

  def prettyprintExp(e: ast.Expression, typeInfo: String): P = e match {
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
      val prefix = s"If$typeInfo(".nl + prettyprintTypedExp(cond).indent + ", ".nl + prettyprintTypedExp(pos).indent
      val els = neg.map(e => ", ".nl + prettyprintTypedExp(e).indent).getOrElse("".p)
      prefix + els + ")"

    case ast.InfixAp(op, left, right) =>
      s"InfixAp$typeInfo($op, ".nl + prettyprintTypedExp(left).indent + ", ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Block(body) =>
      s"Block$typeInfo(".nl + body.map {
        case d: ast.Def => prettyprintDef(d)
        case e: ast.Typed[ast.Expression] => prettyprintTypedExp(e)
      }.sep(",".nl).indent + ")"

    case ast.App(e, ps) =>
      s"App$typeInfo(".nl + (prettyprintTypedExp(e) + ",".nl + "Params(".nl + ps.map {
        expr => prettyprintTypedExp(expr)
      }.sep(",".nl).indent + ")").indent

    case ast.Assign(left, op, right) =>
      s"Assign$typeInfo($op, ".nl + (prettyprintTypedExp(left) + ",".nl + prettyprintTypedExp(right)).indent + ")"

    case ast.PrefixAp(op, right) =>
      s"PrefixApp$typeInfo($op, ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Select(expr, member) =>
      s"Select$typeInfo($member, ".nl + prettyprintTypedExp(expr).indent + ")"

    case ast.While(cond, body) =>
      s"While$typeInfo(".nl + (prettyprintTypedExp(cond) + ",".nl + prettyprintTypedExp(body)).indent + ")"
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

}

object TypedAstPrettyprint extends AstPrettyprint {
  val ast = analyze.ast

  override def prettyprintTypedExp(e: Typed[ast.Expression]): P = e.expr match {

    case ast.IntLit(i) => s"[${e.typ}]($i)"

    case ast.BoolLit(b) => s"[${e.typ}]($b)"

    case ast.StringLit(s) =>
      val str = "\"" + s.flatMap {
        case '\n' => "\\n"
        case '"' => "\\\""
        case '\\' => "\\\\"
        case c => c.toString
      } + "\""
      s"[${e.typ}]($str)"

    case ast.Var(v) => s"[${e.typ}](${prettyprintSymbol(v)})"

    case _ => prettyprintExp(e.expr, s"[${e.typ}]")
  }
}
object ParseAstPrettyprint extends AstPrettyprint {
  val ast = parse.ast

  override def prettyprintTypedExp(e: ast.Expression): P = prettyprintExp(e, "")
}