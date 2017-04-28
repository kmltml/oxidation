package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Val {

  import Val._

  def typ: Type

  def representation: Vector[Byte] = this match {
    case I(v, t) =>
      Vector(
        v & 0xff,
        v >>> 8*1 & 0xff,
        v >>> 8*2 & 0xff,
        v >>> 8*3 & 0xff,
        v >>> 8*4 & 0xff,
        v >>> 8*5 & 0xff,
        v >>> 8*6 & 0xff,
        v >>> 8*7
      ).take(t.size).map(_.toByte)
    case Struct(members) =>
      members.foldMap(_.representation)
    case Array(elems) =>
      elems.foldMap(_.representation)
  }

}

object Val {

  def unapply(v: Val): Option[(Val, Type)] = Some(v, v.typ)

  final case class R(register: Register) extends Val {
    def typ = register.typ
  }
  final case class I(value: Long, typ: Type) extends Val
  final case class G(name: Name, typ: Type) extends Val
  final case class Struct(members: Vector[Val]) extends Val {
    lazy val typ: Type = Type.Struct(members.map(_.typ))
  }
  final case class Array(elems: List[Val]) extends Val {
    lazy val typ: Type.Arr = Type.Arr(elems.headOption.map(_.typ) getOrElse Type.U0, elems.size)
  }
  final case class UArr(typ: Type.Arr) extends Val
  final case class Const(entry: ConstantPoolEntry, typ: Type) extends Val
  final case class GlobalAddr(name: Name) extends Val {
    override def typ: Type = Type.Ptr
  }

  implicit val show: Show[Val] = {
    case R(reg) => reg.show
    case I(value, typ) => show"($value)[$typ]"
    case G(n, typ) => show"(@$n)[$typ]"
    case Struct(members) => members.map(_.show).mkString("{", ", ", "}")
    case Const(e, t) => show"const ($e)[$t]"
    case GlobalAddr(n) => show"(&@$n)[ptr]"
    case arr @ Array(elems) =>
      val Type.Arr(elemType, size) = arr.typ
      show"arr[$elemType, $size](${elems.map(_.show) mkString ", "})"
    case UArr(Type.Arr(elem, size)) => show"uarr[$elem, $size]"
  }

}
