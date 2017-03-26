package oxidation
package ir


import cats._
import cats.data._
import cats.implicits._

trait RegisterNamespace {

  def prefix: String

  override def toString: String = prefix

}

final case class Register(ns: RegisterNamespace, index: Int, typ: Type)

object Register {

  implicit val show: Show[Register] = {
    case Register(ns, i, typ) => show"${ns.prefix}$i[$typ]"
  }

}
