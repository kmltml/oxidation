package oxidation

object ast {

  sealed trait BlockStatement

  sealed trait Expression extends BlockStatement

  final case class IntLit(value: Int) extends Expression
  final case class InfixAp(operator: InfixOp, left: Expression, right: Expression) extends Expression
  final case class PrefixAp(operator: PrefixOp, expr: Expression) extends Expression
  final case class Var(name: String) extends Expression
  final case class Block(body: Seq[BlockStatement]) extends Expression
  final case class Apply(expr: Expression, params: Seq[Expression]) extends Expression
  final case class Select(expr: Expression, member: String) extends Expression
  final case class If(cond: Expression, positive: Expression, negative: Option[Expression]) extends Expression
  final case class While(cond: Expression, body: Expression) extends Expression
  final case class Assign(lval: Expression, op: Option[InfixOp], rval: Expression) extends Expression

  sealed trait Def extends BlockStatement

  final case class DefDef(name: String, params: Option[Seq[Param]], typ: Option[Type], body: Expression) extends Def
  final case class ValDef(name: String, typ: Option[Type], value: Expression) extends Def
  final case class VarDef(name: String, typ: Option[Type], value: Expression) extends Def
  final case class StructDef(name: String, members: Seq[StructMember]) extends Def

  final case class Param(name: String, typ: Type)
  final case class StructMember(name: String, typ: Type)

  sealed trait InfixOp

  object InfixOp {

    case object Add extends InfixOp
    case object Sub extends InfixOp
    case object Div extends InfixOp
    case object Mod extends InfixOp
    case object Mul extends InfixOp
    case object Shl extends InfixOp
    case object Shr extends InfixOp
    case object BitAnd extends InfixOp
    case object BitOr extends InfixOp
    case object Xor extends InfixOp
    case object And extends InfixOp
    case object Or extends InfixOp
    case object Eq extends InfixOp
    case object Lt extends InfixOp
    case object Gt extends InfixOp
    case object Geq extends InfixOp
    case object Leq extends InfixOp
    case object Neq extends InfixOp

  }

  sealed trait PrefixOp

  object PrefixOp {

    case object Neg extends PrefixOp
    case object Not extends PrefixOp
    case object Inv extends PrefixOp

  }

  sealed trait Type

  object Type {

    final case class Named(name: String) extends Type

  }
}
