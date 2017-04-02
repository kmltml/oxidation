package oxidation


import cats._
import cats.data._
import cats.implicits._

trait Ast {

  type Typed[+_]
  type TypeInfo

  sealed trait BlockStatement

  sealed trait Expression extends BlockStatement with Product with Serializable

  final case class IntLit(value: Int) extends Expression
  final case class BoolLit(value: Boolean) extends Expression
  final case class CharLit(value: Char) extends Expression
  final case class StringLit(value: String) extends Expression
  final case class StructLit(name: Symbol, members: Seq[(String, Typed[Expression])]) extends Expression
  final case class UnitLit() extends Expression
  final case class InfixAp(operator: InfixOp, left: Typed[Expression], right: Typed[Expression]) extends Expression
  final case class PrefixAp(operator: PrefixOp, expr: Typed[Expression]) extends Expression
  final case class Var(name: Symbol) extends Expression
  final case class Block(body: Seq[Typed[BlockStatement]]) extends Expression
  final case class App(expr: Typed[Expression], params: Seq[Typed[Expression]]) extends Expression
  final case class Select(expr: Typed[Expression], member: String) extends Expression
  final case class If(cond: Typed[Expression], positive: Typed[Expression], negative: Option[Typed[Expression]]) extends Expression
  final case class While(cond: Typed[Expression], body: Typed[Expression]) extends Expression
  final case class Assign(lval: Typed[Expression], op: Option[InfixOp], rval: Typed[Expression]) extends Expression

  final case class Extern() extends Expression

  // Synthetic expressions (not created by the parser)
  final case class Widen(expr: Typed[Expression]) extends Expression
  final case class Ignore(expr: Typed[Expression]) extends Expression


  sealed trait TLD

  sealed trait Def extends BlockStatement with TLD {
    def name: Symbol
  }

  sealed trait TermDef extends Def {
    def typ: Option[TypeName]
    def body: Typed[Expression]
  }

  final case class DefDef(name: Symbol, params: Option[Seq[Param]], typ: Option[TypeName], body: Typed[Expression]) extends TermDef

  final case class Param(name: String, typ: TypeInfo)

  final case class ValDef(name: Symbol, typ: Option[TypeName], body: Typed[Expression]) extends TermDef
  final case class VarDef(name: Symbol, typ: Option[TypeName], body: Typed[Expression]) extends TermDef

  sealed trait TypeDef extends Def

  final case class StructDef(name: Symbol, typeParameters: Option[Seq[String]], members: Seq[StructMemberDef]) extends TypeDef
  final case class EnumDef(name: Symbol, typeParameters: Option[Seq[String]], variants: Seq[EnumVariant]) extends TypeDef
  final case class TypeAliasDef(name: Symbol, typeParameters: Option[Seq[String]], body: TypeName) extends TypeDef

  final case class Module(path: Seq[String]) extends TLD
  final case class Import(path: Seq[String], names: ImportSpecifier) extends TLD

  def traverse[A](stmnt: Typed[BlockStatement])(f: PartialFunction[Typed[BlockStatement], A]): Vector[A] = {
    val result = f.lift(stmnt)
    val more = stmnt match {
      case _: IntLit | _: StringLit | _: BoolLit | _: Var | _: Extern |
           _: TypeAliasDef | _: StructDef | _: EnumDef | _: CharLit |
           _: UnitLit => Vector.empty[A]
      case InfixAp(_, left, right) => traverse(left)(f) ++ traverse(right)(f)
      case PrefixAp(_, e) => traverse(e)(f)
      case Block(stmnts) => stmnts.toList.foldMap(traverse(_)(f))
      case App(e, params) => traverse(e)(f) ++ params.toList.foldMap(traverse(_)(f))
      case Select(e, _) => traverse(e)(f)
      case If(c, p, n) =>
        traverse(c)(f) ++ traverse(p)(f) ++ n.map(traverse(_)(f)).orEmpty
      case While(c, b) => traverse(c)(f) ++ traverse(b)(f)
      case Assign(l, _, r) => traverse(l)(f) ++ traverse(r)(f)
      case StructLit(_, members) => members.map(_._2).toVector.foldMap(traverse(_)(f))

      case DefDef(_, _, _, b) => traverse(b)(f)
      case ValDef(_, _, v) => traverse(v)(f)
      case VarDef(_, _, v) => traverse(v)(f)

      case Ignore(e) => traverse(e)(f)
    }
    (result ++ more).toVector
  }

  def listTermSymbols(stmnt: Typed[BlockStatement]): Seq[Symbol] = traverse(stmnt) {
    case Var(s) => s
  }

}

final case class StructMemberDef(name: String, typ: TypeName)
final case class EnumVariant(name: String, members: Seq[StructMemberDef])
