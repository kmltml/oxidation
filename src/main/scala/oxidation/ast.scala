package oxidation


import cats._
import cats.data._
import cats.implicits._
import oxidation.parse.Span

trait Ast {

  type Typed[+_]
  type TypeInfo

  protected def extractTyped[A](t: Typed[A]): A

  sealed trait BlockStatement

  sealed trait Expression extends BlockStatement with Product with Serializable {

    def loc: Span

    def withLoc(loc: Span): Expression = this match {
      case x: IntLit => x.copy(loc = loc)
      case x: FloatLit => x.copy(loc = loc)
      case x: BoolLit => x.copy(loc = loc)
      case x: CharLit => x.copy(loc = loc)
      case x: StringLit => x.copy(loc = loc)
      case x: StructLit => x.copy(loc = loc)
      case x: EnumLit => x.copy(loc = loc)
      case x: UnitLit => x.copy(loc = loc)
      case x: InfixAp => x.copy(loc = loc)
      case x: PrefixAp => x.copy(loc = loc)
      case x: Var => x.copy(loc = loc)
      case x: Block => x.copy(loc = loc)
      case x: Method => x.copy(loc = loc)
      case x: App => x.copy(loc = loc)
      case x: TypeApp => x.copy(loc = loc)
      case x: Select => x.copy(loc = loc)
      case x: If => x.copy(loc = loc)
      case x: While => x.copy(loc = loc)
      case x: Match => x.copy(loc = loc)
      case x: Assign => x.copy(loc = loc)
      case x: Extern => x.copy(loc = loc)
      case x: Widen => x.copy(loc = loc)
      case x: Trim => x.copy(loc = loc)
      case x: Convert => x.copy(loc = loc)
      case x: Reinterpret => x.copy(loc = loc)
      case x: Ignore => x.copy(loc = loc)
      case x: Stackalloc => x.copy(loc = loc)
      case x: ArrLit => x.copy(loc = loc)
    }

  }

  final case class IntLit(value: Long, loc: Span) extends Expression {
    def pattern = Pattern.IntLit(value, loc)
  }
  final case class FloatLit(value: BigDecimal, loc: Span) extends Expression {
    def pattern = Pattern.FloatLit(value, loc)
  }
  final case class BoolLit(value: Boolean, loc: Span) extends Expression {
    def pattern = Pattern.BoolLit(value, loc)
  }
  final case class CharLit(value: Char, loc: Span) extends Expression {
    def pattern = Pattern.CharLit(value, loc)
  }
  final case class StringLit(value: String, loc: Span) extends Expression
  final case class StructLit(name: Symbol, members: List[(String, Typed[Expression])], loc: Span) extends Expression
  final case class EnumLit(variantName: String, members: List[(String, Typed[Expression])], loc: Span) extends Expression
  final case class UnitLit(loc: Span) extends Expression
  final case class InfixAp(operator: InfixOp, left: Typed[Expression], right: Typed[Expression], loc: Span) extends Expression
  final case class PrefixAp(operator: PrefixOp, expr: Typed[Expression], loc: Span) extends Expression
  final case class Var(name: Symbol, loc: Span) extends Expression {
    def pattern = Pattern.Var(name, loc)
  }
  final case class Block(body: Vector[Typed[BlockStatement]], loc: Span) extends Expression
  final case class Method(target: Typed[Expression], method: Typed[Expression], loc: Span) extends Expression
  final case class App(expr: Typed[Expression], params: List[Typed[Expression]], loc: Span) extends Expression
  final case class TypeApp(expr: Typed[Expression], params: List[TypeName], loc: Span) extends Expression
  final case class Select(expr: Typed[Expression], member: String, loc: Span) extends Expression
  final case class If(cond: Typed[Expression], positive: Typed[Expression], negative: Option[Typed[Expression]], loc: Span) extends Expression
  final case class While(cond: Typed[Expression], body: Typed[Expression], loc: Span) extends Expression
  final case class Match(matchee: Typed[Expression], cases: List[MatchCase], loc: Span) extends Expression
  final case class Assign(lval: Typed[Expression], op: Option[InfixOp], rval: Typed[Expression], loc: Span) extends Expression

  final case class Extern(loc: Span) extends Expression

  // Synthetic expressions (not created by the parser)
  final case class Widen(expr: Typed[Expression], loc: Span) extends Expression
  final case class Trim(expr: Typed[Expression], loc: Span) extends Expression
  final case class Convert(expr: Typed[Expression], loc: Span) extends Expression
  final case class Reinterpret(expr: Typed[Expression], loc: Span) extends Expression
  final case class Ignore(expr: Typed[Expression], loc: Span) extends Expression
  final case class Stackalloc(pointee: TypeInfo, loc: Span) extends Expression
  final case class ArrLit(elems: List[Typed[Expression]], loc: Span) extends Expression

  final case class MatchCase(pattern: Typed[Pattern], guard: Option[Typed[Expression]], body: Typed[Expression])

  sealed trait Pattern {

    def loc: Span

    /**
      * True if the pattern matches a single element of a set not checked for exhaustivity.
      * If a patern is infinitesimal, that means, that no attempt will be made to check if all cases are covered,
      * there has to be a catch-all case to pass the exhaustivity checker.
      */
    def isInfinitesimal: Boolean = this match {
      case Pattern.IntLit(_, _) | Pattern.FloatLit(_, _) | Pattern.CharLit(_, _) | Pattern.Pin(_, _) => true
      case Pattern.Struct(_, members, _, _) => members.exists(t => extractTyped(t._2).isInfinitesimal)
      case Pattern.Enum(_, members, _, _) => members.exists(t => extractTyped(t._2).isInfinitesimal)
      case Pattern.Or(l, r, _) => extractTyped(l).isInfinitesimal && extractTyped(r).isInfinitesimal
      case _ => false
    }

    def withLoc(loc: Span): Pattern = this match {
      case x: Pattern.Var => x.copy(loc = loc)
      case x: Pattern.Ignore => x.copy(loc = loc)
      case x: Pattern.IntLit => x.copy(loc = loc)
      case x: Pattern.FloatLit => x.copy(loc = loc)
      case x: Pattern.BoolLit => x.copy(loc = loc)
      case x: Pattern.CharLit => x.copy(loc = loc)
      case x: Pattern.Struct => x.copy(loc = loc)
      case x: Pattern.Enum => x.copy(loc = loc)
      case x: Pattern.Or => x.copy(loc = loc)
      case x: Pattern.Alias => x.copy(loc = loc)
      case x: Pattern.Pin => x.copy(loc = loc)
    }

  }

  object Pattern {

    final case class Var(name: Symbol, loc: Span) extends Pattern
    final case class Ignore(loc: Span) extends Pattern
    final case class IntLit(value: Long, loc: Span) extends Pattern
    final case class FloatLit(value: BigDecimal, loc: Span) extends Pattern
    final case class BoolLit(value: Boolean, loc: Span) extends Pattern
    final case class CharLit(value: Char, loc: Span) extends Pattern
    final case class Struct(typeName: Option[Symbol], members: List[(String, Typed[Pattern])], ignoreExtra: Boolean, loc: Span) extends Pattern
    final case class Enum(variant: Symbol, members: List[(String, Typed[Pattern])], ignoreExtra: Boolean, loc: Span) extends Pattern
    final case class Or(left: Typed[Pattern], right: Typed[Pattern], loc: Span) extends Pattern
    final case class Alias(name: Symbol, pattern: Typed[Pattern], loc: Span) extends Pattern
    final case class Pin(subexp: Typed[Expression], loc: Span) extends Pattern

  }

  sealed trait TLD

  sealed trait Def extends BlockStatement with TLD {
    def name: Symbol
  }

  sealed trait TermDef extends Def {
    def typ: Option[TypeName]
    def body: Typed[Expression]
  }

  final case class DefDef(name: Symbol, params: Option[List[Param]], typ: Option[TypeName], body: Typed[Expression]) extends TermDef

  final case class Param(name: String, typ: TypeInfo)

  final case class ValDef(name: Symbol, typ: Option[TypeName], body: Typed[Expression]) extends TermDef
  final case class VarDef(name: Symbol, typ: Option[TypeName], body: Typed[Expression]) extends TermDef

  sealed trait TypeDef extends Def

  final case class StructDef(name: Symbol, typeParameters: Option[List[String]], members: List[StructMemberDef]) extends TypeDef
  final case class EnumDef(name: Symbol, typeParameters: Option[List[String]], variants: List[EnumVariantDef]) extends TypeDef
  final case class TypeAliasDef(name: Symbol, typeParameters: Option[List[String]], body: TypeName) extends TypeDef

  final case class Module(path: List[String]) extends TLD
  final case class Import(path: List[String], names: ImportSpecifier) extends TLD

  def traverse[A](stmnt: Typed[BlockStatement])(f: PartialFunction[Typed[BlockStatement], A]): Vector[A] = {
    val result = f.lift(stmnt)
    val more = extractTyped(stmnt) match {
      case _: IntLit | _: StringLit | _: BoolLit | _: Var | _: Extern |
           _: TypeAliasDef | _: StructDef | _: EnumDef | _: CharLit |
           _: UnitLit | _: FloatLit => Vector.empty[A]
      case InfixAp(_, left, right, _) => traverse(left)(f) ++ traverse(right)(f)
      case PrefixAp(_, e, _) => traverse(e)(f)
      case Block(stmnts, _) => stmnts.foldMap(traverse(_)(f))
      case Method(t, m, _) => traverse(t)(f) ++ traverse(m)(f)
      case App(e, params, _) => traverse(e)(f) ++ params.foldMap(traverse(_)(f))
      case TypeApp(e, _, _) => traverse(e)(f)
      case Select(e, _, _) => traverse(e)(f)
      case If(c, p, n, _) =>
        traverse(c)(f) ++ traverse(p)(f) ++ n.map(traverse(_)(f)).orEmpty
      case While(c, b, _) => traverse(c)(f) ++ traverse(b)(f)
      case Match(m, c, _) =>
        def traversePattern(pattern: Typed[Pattern]): Vector[A] = extractTyped(pattern) match {
          case Pattern.Pin(e, _) => traverse(e)(f)
          case _ => Vector.empty
        }
        traverse(m)(f) ++ c.foldMap { case MatchCase(p, _, e) => traversePattern(p) ++ traverse(m)(f)}
      case Assign(l, _, r, _) => traverse(l)(f) ++ traverse(r)(f)
      case StructLit(_, members, _) => members.map(_._2).foldMap(traverse(_)(f))
      case EnumLit(_, members, _) => members.map(_._2).foldMap(traverse(_)(f))

      case DefDef(_, _, _, b) => traverse(b)(f)
      case ValDef(_, _, v) => traverse(v)(f)
      case VarDef(_, _, v) => traverse(v)(f)

      case Ignore(e, _) => traverse(e)(f)
    }
    (result ++ more).toVector
  }

  def listTermSymbols(stmnt: Typed[BlockStatement]): Vector[Symbol] = traverse(stmnt) {
    case Var(s, _) => s
  }

}

final case class StructMemberDef(name: String, typ: TypeName)
final case class EnumVariantDef(name: Symbol, members: List[StructMemberDef])
