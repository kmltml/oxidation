package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Val

object Val {

  final case class R(register: Register) extends Val
  final case class I(value: Int) extends Val
  final case class G(name: Name) extends Val

  implicit val show: Show[Val] = {
    case R(reg) => reg.show
    case I(value) => value.show
    case G(n) => show"@$n"
  }

}
