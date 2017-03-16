package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Op

object Op {

  final case class Arith(op: InfixOp, left: Val, right: Val) extends Op
  final case class Copy(src: Val) extends Op

  implicit val show: Show[Op] = {
    case Copy(src) => show"$src"
    case Arith(op, left, right) => show"$left $op $right"
  }
}
