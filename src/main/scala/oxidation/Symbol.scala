package oxidation

import cats._
import cats.data._
import cats.implicits._

sealed trait Symbol {

  def name: String

}

object Symbol {

  final case class Unresolved(path: List[String]) extends Symbol {
    def name = path.last
  }
  final case class Global(path: List[String]) extends Symbol {
    require(path.nonEmpty)
    def name: String = path.last
  }
  final case class Local(name: String) extends Symbol
  final case class Specialized(params: List[Symbol], underlying: Symbol) extends Symbol {
    def name = underlying.name ++ "?a" ++ params.mkString("?a")
  }

  implicit val show: Show[Symbol] = {
    case Unresolved(path) => s"?${path mkString ","}"
    case Local(n) => s"$$$n"
    case Global(path) => s"@${path.mkString(",")}"
  }

}
