package oxidation
package codegen

import cats._
import cats.data._
import cats.implicits._

sealed trait Name

object Name {

  final case class Local(prefix: String, index: Int) extends Name
  final case class Global(path: List[String]) extends Name

  implicit val show: Show[Name] = {
    case Local(p, i) => show"$p.$i"
    case Global(path) => path.mkString(".")
  }

}
