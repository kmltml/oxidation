package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Type {

  import Type._

  def size: Int = this match {
    case I(w) => w / 8
    case U(w) => w / 8
    case Ptr => 8
    case U0 | U1 => 1
    case Fun(_, _) => ???
    case Struct(members) => members.map(_.size).sum
    case Arr(member, elemCount) => member.size * elemCount
  }

}

object Type {

  case object U0 extends Type
  case object U1 extends Type
  case object Ptr extends Type

  sealed trait Num extends Type

  sealed abstract class I(val w: Int) extends Num
  object I {
    def unapply(arg: I): Option[Int] = Some(arg.w)
  }
  case object I8 extends I(8)
  case object I16 extends I(16)
  case object I32 extends I(32)
  case object I64 extends I(64)

  sealed abstract class U(val w: Int) extends Num
  object U {
    def unapply(arg: U): Option[Int] = Some(arg.w)
  }
  case object U8 extends U(8)
  case object U16 extends U(16)
  case object U32 extends U(32)
  case object U64 extends U(64)

  final case class Fun(params: List[Type], ret: Type) extends Type

  final case class Struct(members: Vector[Type]) extends Type {

    lazy val layout: Vector[Int] = members.map(_.size).scanLeft(0)(_ + _)

    def offset(index: Int): Int = layout(index)

  }

  final case class Arr(member: Type, elems: Int) extends Type

  implicit val show: Show[Type] = new Show[Type] {
    def show(t: Type): String = t match {
      case Fun(params, ret) => show"(${params.map(show).mkString(", ")}) => ${show(ret)}"
      case Struct(members) => members.map(show).mkString("{", ", ", "}")
      case t => t.toString.toLowerCase
    }
  }

}
