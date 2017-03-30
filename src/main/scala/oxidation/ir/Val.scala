package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Val {

  def typ: Type

}

object Val {

  final case class R(register: Register) extends Val {
    def typ = register.typ
  }
  final case class I(value: Int, typ: Type) extends Val
  final case class G(name: Name, typ: Type) extends Val
  final case class Struct(members: Vector[Val]) extends Val {
    lazy val typ: Type = Type.Struct(members.map(_.typ))
  }
  final case class Const(entry: ConstantPoolEntry, typ: Type) extends Val

  implicit val show: Show[Val] = {
    case R(reg) => reg.show
    case I(value, typ) => show"($value)[$typ]"
    case G(n, typ) => show"(@$n)[$typ]"
    case Struct(members) => members.map(_.show).mkString("{", ", ", "}")
    case Const(e, t) => show"const ($e)[$t]"
  }

}
