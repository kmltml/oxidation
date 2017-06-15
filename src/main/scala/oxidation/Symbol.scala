package oxidation

import cats._
import cats.data._
import cats.implicits._

sealed trait Symbol {

  def name: String

}

object Symbol {

  final case class Unresolved(name: String) extends Symbol
  final case class Global(path: List[String]) extends Symbol {
    require(path.nonEmpty)
    def name: String = path.last
  }
  final case class Local(name: String) extends Symbol

  implicit val show: Show[Symbol] = {
    case Unresolved(n) => s"?$n"
    case Local(n) => s"$$$n"
    case Global(path) => s"@${path.mkString(",")}"
  }

}
