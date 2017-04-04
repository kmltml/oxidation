package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Op {

  def reads: Set[Register] = {
    val vals = this match {
      case Op.Arith(_, l, r) => Set(l, r)
      case Op.Call(fn, params) => params.map(ir.Val.R).toSet + fn
      case Op.Copy(v) => Set(v)
      case Op.Unary(_, v) => Set(v)
      case Op.Garbled => Set.empty
      case Op.Load(a, o) => Set(a, o)
      case Op.Store(a, o, v) => Set(a, o, v)
      case Op.Widen(v) => Set(v)
      case Op.Trim(v) => Set(v)
      case Op.Member(s, _) => Set(s)
    }
    vals.collect {
      case Val.R(r) => r
    }
  }

}

object Op {

  final case class Arith(op: InfixOp, left: Val, right: Val) extends Op
  final case class Copy(src: Val) extends Op
  final case class Call(fn: Val, params: List[Register]) extends Op
  final case class Unary(op: PrefixOp, right: Val) extends Op
  final case class Load(addr: Val, offset: Val) extends Op
  final case class Store(addr: Val, offset: Val, value: Val) extends Op
  final case class Widen(v: Val) extends Op
  final case class Trim(v: Val) extends Op
  final case class Member(src: Val, index: Int) extends Op
  case object Garbled extends Op // Assigned to register to indicate, that some instruction also writes to this register as a side effect

  implicit val show: Show[Op] = {
    case Copy(src) => show"$src"
    case Arith(op, left, right) => show"$left $op $right"
    case Unary(op, right) => show"$op $right"
    case Call(fn, params) => show"call $fn (${params.map(_.show).mkString(", ")})"
    case Load(addr, offset) => show"load [$addr + $offset]"
    case Store(addr, offset, value) => show"store [$addr + $offset] = $value"
    case Widen(v) => show"widen $v"
    case Trim(v) => show"trim $v"
    case Member(s, i) => show"member $s.$i"
    case Garbled => "garbled"
  }
}
