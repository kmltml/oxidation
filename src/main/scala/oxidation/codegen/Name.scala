package oxidation
package codegen

import cats._
import cats.data._
import cats.implicits._

final case class Name(prefix: String, index: Int)

object Name {

  implicit val show: Show[Name] = {
    case Name(p, i) => show"$p.$i"
  }

}
