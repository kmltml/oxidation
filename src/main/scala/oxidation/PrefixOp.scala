package oxidation

sealed trait PrefixOp

object PrefixOp {

  case object Neg extends PrefixOp
  case object Not extends PrefixOp
  case object Inv extends PrefixOp

}
