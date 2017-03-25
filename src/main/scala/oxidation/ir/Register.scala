package oxidation
package ir


import cats._
import cats.data._
import cats.implicits._

final case class Register(index: Int, typ: Type)

object Register {

  implicit val show: Show[Register] = {
    case Register(i, typ) => show"r$i[$typ]"
  }

}
