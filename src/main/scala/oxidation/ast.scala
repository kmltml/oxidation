package oxidation

trait Ast {

  type Typed[_]

  sealed trait BlockStatement

  sealed trait Expression extends BlockStatement

  final case class IntLit(value: Int) extends Expression
  final case class BoolLit(value: Boolean) extends Expression
  final case class StringLit(value: String) extends Expression
  final case class InfixAp(operator: InfixOp, left: Typed[Expression], right: Typed[Expression]) extends Expression
  final case class PrefixAp(operator: PrefixOp, expr: Typed[Expression]) extends Expression
  final case class Var(name: String) extends Expression
  final case class Block(body: Seq[Typed[BlockStatement]]) extends Expression
  final case class App(expr: Typed[Expression], params: Seq[Typed[Expression]]) extends Expression
  final case class Select(expr: Typed[Expression], member: String) extends Expression
  final case class If(cond: Typed[Expression], positive: Typed[Expression], negative: Option[Typed[Expression]]) extends Expression
  final case class While(cond: Typed[Expression], body: Typed[Expression]) extends Expression
  final case class Assign(lval: Typed[Expression], op: Option[InfixOp], rval: Typed[Expression]) extends Expression

  sealed trait Def extends BlockStatement

  final case class DefDef(name: String, params: Option[Seq[Param]], typ: Option[Type], body: Typed[Expression]) extends Def
  final case class ValDef(name: String, typ: Option[Type], value: Typed[Expression]) extends Def
  final case class VarDef(name: String, typ: Option[Type], value: Typed[Expression]) extends Def
  final case class StructDef(name: String, members: Seq[StructMember]) extends Def

  final case class Param(name: String, typ: Type)
  final case class StructMember(name: String, typ: Type)

  sealed trait Type

  object Type {

    final case class Named(name: String) extends Type
    final case class App(const: Type, params: Seq[Type]) extends Type

  }
}
