package oxidation
package codegen
package ir


import cats._
import cats.data._
import cats.implicits._

final case class Register(index: Int)

object Register {

  implicit val show: Show[Register] = {
    case Register(i) => show"r$i"
  }

}
