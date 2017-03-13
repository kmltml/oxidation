package oxidation

import cats._
import cats.data._
import cats.implicits._

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

  implicit val show: Show[InfixOp] = {
    case Add => "+"
    case Sub => "-"
    case Div => "/"
    case Mod => "%"
    case Mul => "*"
    case Shl => "<<"
    case Shr => ">>"
    case BitAnd => "&"
    case BitOr => "|"
    case Xor => "^"
    case And => "&&"
    case Or => "||"
    case Eq => "=="
    case Lt => "<"
    case Gt => ">"
    case Geq => ">="
    case Leq => "<="
    case Neq => "!="
  }

}
