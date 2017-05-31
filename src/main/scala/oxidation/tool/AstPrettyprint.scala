package oxidation
package tool
import oxidation.analyze.{Type, Typed}

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
      s"ValDef(${prettyprintSymbol(name)}, ${tpe.map(prettyprintTypeName)},".nl + prettyprintTypedExp(value).indent + ")"

    case ast.VarDef(name, tpe, value) =>
      s"VarDef(${prettyprintSymbol(name)}, ${tpe.map(prettyprintTypeName)},".nl + prettyprintTypedExp(value).indent + ")"

    case ast.DefDef(name, params, tpe, body) =>
      val pars = params match {
        case Some(p) => "Params(".nl + p.map {
          case ast.Param(n, t) => s"Param($n, ".p + prettyprintTypeInfo(t) + ")"
        }.sep(", ".nl).indent + ")"
        case None => "None".p
      }
      val retType = tpe match {
        case Some(t) => prettyprintTypeName(t)
        case None => "None"
      }
      s"DefDef(${prettyprintSymbol(name)}, ".nl + (pars + ",".nl + retType + ",".nl + prettyprintTypedExp(body)).indent + ")"

    case ast.StructDef(name, typeParams, members) =>
      val typeParamList = typeParams match {
        case Some(ps) => s"[${ps mkString ", "}]"
        case None => ""
      }
      s"StructDef(${prettyprintSymbol(name)}$typeParamList, ".nl + members.map {
        case StructMemberDef(name, tpe) => s"Member($name, ".p + prettyprintTypeName(tpe) + ")"
      }.sep(", ".nl).indent + ")"

    case ast.TypeAliasDef(name, typeParams, body) =>
      val typeParamList = typeParams match {
        case Some(ps) => s"[${ps mkString ", "}]"
        case None => ""
      }
      s"TypeDef(${prettyprintSymbol(name)}$typeParamList, ${prettyprintTypeName(body)})"
  }

  def prettyprintTypeInfo(t: ast.TypeInfo): P

  def prettyprintTypeName(t: TypeName): String = t match {
    case TypeName.Named(n) => prettyprintSymbol(n)
    case TypeName.App(t, p) => prettyprintTypeName(t) + "[" + p.map(prettyprintTypeName).mkString(", ") + "]"
  }

  def prettyprintSymbol(s: Symbol): String = s match {
    case Symbol.Local(n) => s"$$$n"
    case Symbol.Global(path) => s"@${path.mkString(".")}"
    case Symbol.Unresolved(n) => s"?$n"
  }

  def prettyprintTypedExp(e: ast.Typed[ast.Expression]): P

  def prettyprintTypedBlockStatement(e: ast.Typed[ast.BlockStatement]): P

  def prettyprintExp(e: ast.Expression, typeInfo: String): P = e match {
    case ast.IntLit(i) => i.toString

    case ast.BoolLit(b) => b.toString

    case ast.FloatLit(f) => f.toString

    case ast.CharLit(c) => c.toString

    case ast.Extern() => s"Extern$typeInfo"

    case ast.UnitLit() => s"Unit$typeInfo"

    case ast.StringLit(s) => "\"" + s.flatMap {
      case '\n' => "\\n"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c.toString
    } + "\""

    case ast.StructLit(name, members) =>
      val memberpp = members.map {
        case (name, exp) => s"($name, ".nl + (prettyprintTypedExp(exp) + ")").indent
      }.sep(",".nl)
      s"StructLit$typeInfo(".p + prettyprintSymbol(name) + ",".nl +
        ("Members(".nl + memberpp.indent + ")").indent

    case ast.Var(v) => prettyprintSymbol(v)

    case ast.If(cond, pos, neg) =>
      val prefix = s"If$typeInfo(".nl + prettyprintTypedExp(cond).indent + ", ".nl + prettyprintTypedExp(pos).indent
      val els = neg.map(e => ", ".nl + prettyprintTypedExp(e).indent).getOrElse("".p)
      prefix + els + ")"

    case ast.InfixAp(op, left, right) =>
      s"InfixAp$typeInfo($op, ".nl + prettyprintTypedExp(left).indent + ", ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Block(body) =>
      s"Block$typeInfo(".nl + body.map(prettyprintTypedBlockStatement).sep(",".nl).indent + ")"

    case ast.App(e, ps) =>
      s"App$typeInfo(".nl + (prettyprintTypedExp(e) + ",".nl + "Params(".nl + ps.map {
        expr => prettyprintTypedExp(expr)
      }.sep(",".nl).indent + ")").indent

    case ast.TypeApp(e, ps) =>
      s"TypeApp$typeInfo(".nl + (prettyprintTypedExp(e) + ",".nl + "Params(".nl + ps.map {
        tn => prettyprintTypeName(tn).p
      }.sep(",".nl).indent + ")").indent

    case ast.Assign(left, op, right) =>
      s"Assign$typeInfo($op, ".nl + (prettyprintTypedExp(left) + ",".nl + prettyprintTypedExp(right)).indent + ")"

    case ast.PrefixAp(op, right) =>
      s"PrefixApp$typeInfo($op, ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Select(expr, member) =>
      s"Select$typeInfo($member, ".nl + prettyprintTypedExp(expr).indent + ")"

    case ast.While(cond, body) =>
      s"While$typeInfo(".nl + (prettyprintTypedExp(cond) + ",".nl + prettyprintTypedExp(body)).indent + ")"

    case ast.Widen(expr) =>
      s"Widen$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Trim(expr) =>
      s"Trim$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Convert(expr) =>
      s"Convert$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Reinterpret(expr) =>
      s"Reinterpret$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Ignore(expr) =>
      s"Ignore$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Stackalloc(pointee) =>
      s"Stackalloc$typeInfo(".p + prettyprintTypeInfo(pointee) + ")"
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

  override def prettyprintTypedExp(e: Typed[ast.Expression]): P = {
    val typ = prettyPrintType(e.typ)
    e.expr match {

      case ast.IntLit(i) => s"[$typ]($i)"

      case ast.FloatLit(i) => s"[$typ]($i)"

      case ast.BoolLit(b) => s"[$typ]($b)"

      case ast.StringLit(s) =>
        val str = "\"" + s.flatMap {
          case '\n' => "\\n"
          case '"' => "\\\""
          case '\\' => "\\\\"
          case c => c.toString
        } + "\""
        s"[$typ]($str)"

      case ast.Var(v) => s"[$typ](${prettyprintSymbol(v)})"

      case _ => prettyprintExp(e.expr, s"[$typ]")
    }
  }

  override def prettyprintTypedBlockStatement(e: Typed[ast.BlockStatement]): P = e match {
    case Typed(d: ast.Def, _) => prettyprintDef(d)
    case Typed(expr: ast.Expression, typ) => prettyprintTypedExp(Typed(expr, typ))
  }

  private def prettyPrintType(t: Type): String = t match {
    case Type.Fun(params, ret) =>
      val p = params.map(prettyPrintType).mkString("(", ", ", ")")
      val r = prettyPrintType(ret)
      s"$p => $r"
    case Type.Struct(name, members) =>
      val n = prettyprintSymbol(name)
      val m = members.map {
        case Type.StructMember(name, typ) => s"$name: ${prettyPrintType(typ)}"
      }.mkString("{", ", ", "}")
      s"$n$m"
    case Type.Ptr(Type.Struct(name, _)) => s"ptr[${prettyprintSymbol(name)}]"
    case Type.Ptr(pointee) => s"ptr[${prettyPrintType(pointee)}]"
    case _ => t.toString
  }

  override def prettyprintTypeInfo(t: Type): P = prettyPrintType(t)
}
object ParseAstPrettyprint extends AstPrettyprint {
  val ast = parse.ast

  override def prettyprintTypedExp(e: ast.Expression): P = prettyprintExp(e, "")

  override def prettyprintTypedBlockStatement(e: ast.BlockStatement): P = e match {
    case d: ast.Def => prettyprintDef(d)
    case e: ast.Expression => prettyprintExp(e, "")
  }

  override def prettyprintTypeInfo(t: TypeName): P = prettyprintTypeName(t)
}
