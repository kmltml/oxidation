package oxidation

import cats._
import cats.data._
import cats.implicits._

sealed trait PrefixOp

object PrefixOp {

  case object Neg extends PrefixOp
  case object Not extends PrefixOp
  case object Inv extends PrefixOp

  implicit val show: Show[PrefixOp] = {
    case Neg => "-"
    case Not => "!"
    case Inv => "~"
  }

}
