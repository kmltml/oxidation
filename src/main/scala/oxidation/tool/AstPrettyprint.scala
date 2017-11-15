package oxidation
package tool
import oxidation.analyze.{Type, Typed}
import cats._
import cats.data._
import cats.implicits._
import oxidation.parse.IndexTranslator

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

  def prettyprintTLD(d: ast.TLD)(implicit translator: IndexTranslator): P = d match {
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

  def prettyprintDef(d: ast.Def)(implicit translator: IndexTranslator): P = d match {
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


    case ast.EnumDef(name, typeParams, variants) =>
      val typeParamList = typeParams match {
        case Some(ps) => s"[${ps mkString ", "}]"
        case None => ""
      }
      s"EnumDef(${prettyprintSymbol(name)}$typeParamList, ".nl + variants.map {
        case EnumVariantDef(name, Nil) =>
          "Variant(".p + prettyprintSymbol(name) + ")"
        case EnumVariantDef(name, members) =>
          "Variant(".p + prettyprintSymbol(name) + ",".nl + members.map {
            case StructMemberDef(name, typ) => s"Member($name, ".p + prettyprintTypeName(typ) + ")"
          }.sep(", ".nl).indent + ")"
      }.sep(",".nl).indent + ")"

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
    case TypeName.IntLiteral(v) => v.toString
  }

  def prettyprintSymbol(s: Symbol): String = s match {
    case Symbol.Local(n) => s"$$$n"
    case Symbol.Global(path) => s"@${path.mkString(".")}"
    case Symbol.Unresolved(n) => s"?$n"
  }

  def prettyprintTypedExp(e: ast.Typed[ast.Expression])(implicit translator: IndexTranslator): P

  def prettyprintTypedBlockStatement(e: ast.Typed[ast.BlockStatement])(implicit translator: IndexTranslator): P

  def prettyprintExp(e: ast.Expression, typeInfo: String)(implicit translator: IndexTranslator): P = e match {
    case ast.IntLit(i, loc) => i.toString + show"{$loc}"

    case ast.BoolLit(b, loc) => b.toString + show"{$loc}"

    case ast.FloatLit(f, loc) => f.toString + show"{$loc}"

    case ast.CharLit(c, loc) => c.toString + show"{$loc}"

    case ast.Extern(loc) => show"Extern{$loc}$typeInfo"

    case ast.UnitLit(loc) => show"Unit{$loc}$typeInfo"

    case ast.StringLit(s, loc) => "\"" + s.flatMap {
      case '\n' => "\\n"
      case '"' => "\\\""
      case '\\' => "\\\\"
      case c => c.toString
    } + "\"" + show"{$loc}"

    case ast.StructLit(name, params, members, loc) =>
      val memberpp = members.map {
        case (name, exp) => s"($name, ".nl + (prettyprintTypedExp(exp) + ")").indent
      }.sep(",".nl)
      val paramspp = params match {
        case None => "".p
        case Some(ps) => "TypeParams(".p + ps.map(prettyprintTypeName).mkString(", ") + ")".nl
      }
      show"StructLit{$loc}$typeInfo(".p + prettyprintSymbol(name) + ",".nl +
        (paramspp + "Members(".nl + memberpp.indent + "))").indent

    case ast.EnumLit(name, members, loc) =>
      val memberpp = members.map {
        case (name, exp) => s"($name, ".nl + (prettyprintTypedExp(exp) + ")").indent
      }.sep(",".nl)
      show"EnumLit{$loc}$typeInfo(".p + name + ",".nl +
        ("Members(".nl + memberpp.indent + "))").indent

    case ast.Var(v, loc) => prettyprintSymbol(v) + show"{$loc}"

    case ast.If(cond, pos, neg, loc) =>
      val prefix = show"If{$loc}$typeInfo(".nl + prettyprintTypedExp(cond).indent + ", ".nl + prettyprintTypedExp(pos).indent
      val els = neg.map(e => ", ".nl + prettyprintTypedExp(e).indent).getOrElse("".p)
      prefix + els + ")"

    case ast.Match(matchee, cases, loc) =>
      show"Match{$loc}$typeInfo(".nl + (prettyprintTypedExp(matchee) + ",".nl + cases.map {
        case ast.MatchCase(pattern, guard, body) =>
          val guardPP = guard.map(g => "Guard(".p + prettyprintTypedExp(g) + "),".nl).getOrElse("".p)
          "Case(".nl + (prettyPrintTypedPattern(pattern) + ",".nl + guardPP +
            prettyprintTypedExp(body) + ")").indent
      }.sep(",".nl) + ")").indent

    case ast.InfixAp(op, left, right, loc) =>
      show"InfixAp{$loc}$typeInfo($op, ".nl + prettyprintTypedExp(left).indent + ", ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Block(body, loc) =>
      show"Block{$loc}$typeInfo(".nl + body.map(prettyprintTypedBlockStatement).sep(",".nl).indent + ")"

    case ast.App(e, ps, loc) =>
      show"App{$loc}$typeInfo(".nl + (prettyprintTypedExp(e) + ",".nl + "Params(".nl + ps.map {
        expr => prettyprintTypedExp(expr)
      }.sep(",".nl).indent + ")").indent

    case ast.TypeApp(e, ps, loc) =>
      show"TypeApp{$loc}$typeInfo(".nl + (prettyprintTypedExp(e) + ",".nl + "Params(".nl + ps.map {
        tn => prettyprintTypeName(tn).p
      }.sep(",".nl).indent + ")").indent

    case ast.Assign(left, op, right, loc) =>
      show"Assign{$loc}$typeInfo($op, ".nl + (prettyprintTypedExp(left) + ",".nl + prettyprintTypedExp(right)).indent + ")"

    case ast.PrefixAp(op, right, loc) =>
      show"PrefixApp{$loc}$typeInfo($op, ".nl + prettyprintTypedExp(right).indent + ")"

    case ast.Select(expr, member, loc) =>
      show"Select{$loc}$typeInfo($member, ".nl + prettyprintTypedExp(expr).indent + ")"

    case ast.While(cond, body, loc) =>
      show"While{$loc}$typeInfo(".nl + (prettyprintTypedExp(cond) + ",".nl + prettyprintTypedExp(body)).indent + ")"

    case ast.Widen(expr, loc) =>
      show"Widen{$loc}$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Trim(expr, loc) =>
      show"Trim{$loc}$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Convert(expr, loc) =>
      show"Convert{$loc}$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Reinterpret(expr, loc) =>
      show"Reinterpret{$loc}$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Ignore(expr, loc) =>
      show"Ignore{$loc}$typeInfo(".p + prettyprintTypedExp(expr) + ")"

    case ast.Stackalloc(pointee, loc) =>
      show"Stackalloc{$loc}$typeInfo(".p + prettyprintTypeInfo(pointee) + ")"

    case ast.ArrLit(elems, loc) =>
      show"ArrLit{$loc}$typeInfo(".nl + elems.map(prettyprintTypedExp).sep(",".nl).indent + ")"
  }

  def prettyPrintTypedPattern(pattern: ast.Typed[ast.Pattern])(implicit translator: IndexTranslator): P

  def prettyPrintPattern(p: ast.Pattern, typeInfo: String)(implicit translator: IndexTranslator): P = p match {
    case ast.Pattern.Var(name, loc) =>
      show"Var{$loc}$typeInfo(".p + prettyprintSymbol(name) + ")"
    case ast.Pattern.Ignore(loc) =>
      show"Ignore{$loc}"

    case ast.Pattern.IntLit(i, loc) => i.toString + show"{$loc}"

    case ast.Pattern.BoolLit(b, loc) => b.toString + show"{$loc}"

    case ast.Pattern.FloatLit(f, loc) => f.toString + show"{$loc}"

    case ast.Pattern.CharLit(c, loc) => c.toString + show"{$loc}"

    case ast.Pattern.Struct(typeName, members, ignoreExtra, loc) =>
      show"Struct{$loc}(".nl +
        (typeName.map(prettyprintSymbol(_).p + ",".nl).getOrElse("".p) +
        members.map {
          case (n, p) => s"Member($n, ".p + prettyPrintTypedPattern(p) + ")".p
        }.sep(",".nl) +
          (if(ignoreExtra) "IgnoreExtra)" else ")").p).indent

    case ast.Pattern.Enum(variant, members, ignoreExtra, loc) =>
      show"Struct{$loc}(".nl +
        prettyprintSymbol(variant).p + ",".nl +
        (members.map {
          case (n, p) => s"Member($n, ".p + prettyPrintTypedPattern(p) + ")".p
        }.sep(",".nl) +
          (if(ignoreExtra) "IgnoreExtra)" else ")").p).indent

    case ast.Pattern.Or(left, right, loc) =>
      show"Or{$loc}(".nl +
        (prettyPrintTypedPattern(left) + ",".nl +
        prettyPrintTypedPattern(right) + ")").indent

    case ast.Pattern.Alias(name, pattern, loc) =>
      show"Alias{$loc}(".nl +
        (prettyprintSymbol(name).p + ",".nl +
          prettyPrintTypedPattern(pattern) + ")").indent

    case ast.Pattern.Pin(subexp, loc) =>
      show"Pin{$loc}(".p + prettyprintTypedExp(subexp) + ")"
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

  override def prettyprintTypedExp(e: Typed[ast.Expression])(implicit translator: IndexTranslator): P = {
    val typ = prettyPrintType(e.typ)
    e.expr match {

      case ast.IntLit(i, loc) => show"{$loc}[$typ]($i)"

      case ast.FloatLit(i, loc) => show"{$loc}[$typ]($i)"

      case ast.BoolLit(b, loc) => show"{$loc}[$typ]($b)"

      case ast.StringLit(s, loc) =>
        val str = "\"" + s.flatMap {
          case '\n' => "\\n"
          case '"' => "\\\""
          case '\\' => "\\\\"
          case c => c.toString
        } + "\""
        show"{$loc}[$typ]($str)"

      case ast.Var(v, loc) => show"{$loc}[$typ](${prettyprintSymbol(v)})"

      case _ => prettyprintExp(e.expr, s"[$typ]")
    }
  }

  override def prettyPrintTypedPattern(p: Typed[ast.Pattern])(implicit translator: IndexTranslator): P = {
    val typ = prettyPrintType(p.typ)
    p.expr match {
      case ast.Pattern.IntLit(i, loc) => show"{$loc}[$typ]($i)"

      case ast.Pattern.FloatLit(i, loc) => show"{$loc}[$typ]($i)"

      case ast.Pattern.BoolLit(b, loc) => show"{$loc}[$typ]($b)"

      case _ => prettyPrintPattern(p.expr, s"[$typ]")
    }
  }


  override def prettyprintTypedBlockStatement(e: Typed[ast.BlockStatement])(implicit translator: IndexTranslator): P = e match {
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
    case Type.Ptr(Type.Enum(name, _)) => s"ptr[${prettyprintSymbol(name)}]"
    case Type.Ptr(pointee) => s"ptr[${prettyPrintType(pointee)}]"
    case Type.Enum(name, variants) =>
      val n = prettyprintSymbol(name)
      val vs = variants.map {
        case Type.EnumVariant(name, members) =>
          val n = prettyprintSymbol(name)
          val m = members.map {
            case Type.StructMember(name, typ) => s"$name: ${prettyPrintType(typ)}"
          }.mkString("{", ", ", "}")
          s"$n$m"
      }.mkString("(", "|", ")")
      s"$n$vs"
    case Type.EnumConstructor(_, Type.EnumVariant(variantName, members)) =>
      val m = members.map {
        case Type.StructMember(name, typ) => s"$name: ${prettyPrintType(typ)}"
      }.mkString("{", ", ", "}")
      s"constructor[$m => ${prettyprintSymbol(variantName)}]"

    case _ => t.toString
  }

  override def prettyprintTypeInfo(t: Type): P = prettyPrintType(t)

}
object ParseAstPrettyprint extends AstPrettyprint {
  val ast = parse.ast

  override def prettyprintTypedExp(e: ast.Expression)(implicit translator: IndexTranslator): P = prettyprintExp(e, "")

  override def prettyPrintTypedPattern(p: ast.Pattern)(implicit translator: IndexTranslator): P = prettyPrintPattern(p, "")

  override def prettyprintTypedBlockStatement(e: ast.BlockStatement)(implicit translator: IndexTranslator): P = e match {
    case d: ast.Def => prettyprintDef(d)
    case e: ast.Expression => prettyprintExp(e, "")
  }

  override def prettyprintTypeInfo(t: TypeName): P = prettyprintTypeName(t)
}
