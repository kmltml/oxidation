package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Op {

  def reads: Set[Register] = {
    val vals = this match {
      case oxidation.ir.Op.Arith(_, l, r) => Set(l, r)
      case oxidation.ir.Op.Call(fn, params) => params.map(ir.Val.R).toSet + fn
      case oxidation.ir.Op.Copy(v) => Set(v)
      case oxidation.ir.Op.Unary(_, v) => Set(v)
    }
    vals.collect {
      case oxidation.ir.Val.R(r) => r
    }
  }

}

object Op {

  final case class Arith(op: InfixOp, left: Val, right: Val) extends Op
  final case class Copy(src: Val) extends Op
  final case class Call(fn: Val, params: List[Register]) extends Op
  final case class Unary(op: PrefixOp, right: Val) extends Op

  implicit val show: Show[Op] = {
    case Copy(src) => show"$src"
    case Arith(op, left, right) => show"$left $op $right"
    case Unary(op, right) => show"$op $right"
    case Call(fn, params) => show"call $fn (${params.map(_.show).mkString(", ")})"
  }
}
