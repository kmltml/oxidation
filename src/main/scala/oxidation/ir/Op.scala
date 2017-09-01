package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Op {

  def reads: Set[Register] = vals.flatMap(_.reads)

  def vals: Set[Val] = this match {
    case Op.Binary(_, l, r) => Set(l, r)
    case Op.Call(fn, params) => params.toSet + fn
    case Op.Copy(v) => Set(v)
    case Op.Unary(_, v) => Set(v)
    case Op.Garbled => Set.empty
    case Op.Load(a, o) => Set(a, o)
    case Op.Store(a, o, v) => Set(a, o, v)
    case Op.Widen(v) => Set(v)
    case Op.Trim(v) => Set(v)
    case Op.Convert(v, _) => Set(v)
    case Op.Reinterpret(v, _) => Set(v)
    case Op.Member(s, _) => Set(s)
    case Op.Elem(a, i) => Set(a, i)
    case Op.ArrStore(a, i, v) => Set(a, i, v)
    case Op.Stackalloc(_) => Set.empty
    case Op.StructCopy(v, s) => s.values.toSet + v
    case Op.Sqrt(v) => Set(v)
    case Op.TagOf(v) => Set(v)
    case Op.Unpack(v, _) => Set(v)
    case Op.Phi(s) => s.values.map(Val.R).toSet
  }

  def isPure: Boolean = this match {
    case Op.Binary(_, _, _) | Op.Copy(_) | Op.Unary(_, _) | Op.Garbled | Op.Load(_, _) |
         Op.Widen(_) | Op.Trim(_) | Op.Convert(_, _) | Op.Reinterpret(_, _) |
         Op.Member(_, _) | Op.Elem(_, _) | Op.Stackalloc(_) | Op.StructCopy(_, _) |
         Op.Sqrt(_) | Op.TagOf(_) | Op.Unpack(_, _) | Op.Phi(_) => true

    case Op.Store(_, _, _) | Op.ArrStore(_, _, _) | Op.Call(_, _) => false
  }

}

object Op {

  final case class Binary(op: InfixOp, left: Val, right: Val) extends Op
  final case class Copy(src: Val) extends Op
  final case class Call(fn: Val, params: List[Val]) extends Op
  final case class Unary(op: PrefixOp, right: Val) extends Op
  final case class Load(addr: Val, offset: Val) extends Op
  final case class Store(addr: Val, offset: Val, value: Val) extends Op
  final case class Widen(v: Val) extends Op
  final case class Trim(v: Val) extends Op
  final case class Convert(v: Val, to: Type) extends Op
  final case class Reinterpret(v: Val, as: Type) extends Op
  final case class Member(src: Val, index: Int) extends Op
  final case class Stackalloc(size: Int) extends Op
  final case class StructCopy(src: Val, substs: Map[Int, Val]) extends Op
  final case class Elem(arr: Val, index: Val) extends Op
  final case class ArrStore(arr: Val, index: Val, value: Val) extends Op
  final case class Sqrt(src: Val) extends Op
  final case class TagOf(src: Val) extends Op
  final case class Unpack(src: Val, variant: Int) extends Op
  final case class Phi(srcs: Map[Name, Register]) extends Op
  case object Garbled extends Op // Assigned to register to indicate, that some instruction also writes to this register as a side effect

  implicit val show: Show[Op] = {
    case Copy(src) => show"$src"
    case Binary(op, left, right) => show"$left $op $right"
    case Unary(op, right) => show"$op $right"
    case Call(fn, params) => show"call $fn (${params.map(_.show).mkString(", ")})"
    case Load(addr, offset) => show"load [$addr + $offset]"
    case Store(addr, offset, value) => show"store [$addr + $offset] = $value"
    case Widen(v) => show"widen $v"
    case Trim(v) => show"trim $v"
    case Convert(v, t) => show"convert[$t] $v"
    case Reinterpret(v, t) => show"reinterpret[$t] $v"
    case Member(s, i) => show"member $s.$i"
    case Stackalloc(s) => show"stackalloc $s"
    case StructCopy(src, substs) => show"structcopy $src { ${substs.map{ case (k, v) => show".$k -> $v" } mkString ", " } }"
    case Elem(arr, index) => show"elem $arr ($index) "
    case ArrStore(arr, index, value) => show"arrstore $arr ($index) = $value"
    case Garbled => "garbled"
    case Sqrt(v) => show"sqrt($v)"
    case TagOf(v) => show"tagof($v)"
    case Unpack(v, t) => show"unpack.$t($v)"
    case Phi(s) =>
      val branches = s.map { case (n, r) => show"$n -> $r"}.mkString(", ")
      show"phi $branches"
  }
}
