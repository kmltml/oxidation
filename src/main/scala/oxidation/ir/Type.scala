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
    case F(w) => w / 8
    case Ptr => 8
    case U0 | U1 => 1
    case Fun(_, _) => ???
    case Struct(members) => members.map(_.size).sum
    case e @ Enum(variants) => variants.map(_.size).max + e.tagType.size
    case Arr(member, elemCount) => member.size * elemCount
  }

}

object Type {

  case object U0 extends Type
  case object U1 extends Type
  case object Ptr extends Type

  sealed trait Num extends Type

  sealed trait Integral extends Num {
    def w: Int
  }

  sealed abstract class I(val w: Int) extends Integral
  object I {
    def unapply(arg: I): Option[Int] = Some(arg.w)
  }
  case object I8 extends I(8)
  case object I16 extends I(16)
  case object I32 extends I(32)
  case object I64 extends I(64)

  sealed abstract class U(val w: Int) extends Integral
  object U {
    def unapply(arg: U): Option[Int] = Some(arg.w)
  }
  case object U8 extends U(8)
  case object U16 extends U(16)
  case object U32 extends U(32)
  case object U64 extends U(64)

  sealed abstract class F(val bits: Int) extends Num
  object F {
    def unapply(arg: F): Option[Int] = Some(arg.bits)
  }

  case object F32 extends F(32)
  case object F64 extends F(64)

  final case class Fun(params: List[Type], ret: Type) extends Type

  final case class Struct(members: Vector[Type]) extends Type {

    lazy val layout: Vector[Int] = members.map(_.size).scanLeft(0)(_ + _)

    def offset(index: Int): Int = layout(index)

  }

  final case class Arr(member: Type, elems: Int) extends Type

  final case class Enum(variants: List[Struct]) extends Type {

    lazy val tagType: Type = {
      val variantCount = variants.size
      if(variantCount <= (1 << 8)) U8
      else if(variantCount <= (1 << 16)) U16
      else if(variantCount <= (1 << 32)) U32
      else U64
    }

    lazy val repr: Struct = {
      val memberCount = (variants.map(_.size + tagType.size).max + 7) / 8
      Struct(Vector.fill(memberCount)(U64))
    }

    lazy val (mappings: Vector[PackedStruct], tagLoc: PackedAtom) = {
      case class AllocState(members: Vector[AllocMember])
      case class AllocMember(bytesAvailable: Int, bytesTaken: Int) {
        def -(bytes: Int): AllocMember = AllocMember(bytesAvailable - bytes, bytesTaken + bytes)
      }
      type S[A] = State[AllocState, A]
      val S = implicitly[MonadState[S, AllocState]]

      def allocAtom(bytes: Int): S[PackedAtom] = State {
        case AllocState(members) =>
          val (member, memberIndex) = members.zipWithIndex
            .sortBy(_._1.bytesTaken) // prefer members without taken bytes to avoid shifting
            .find(bytes <= _._1.bytesAvailable)
            .get

          val newState = AllocState(members.updated(memberIndex, member - bytes))
          val loc = PackedAtom(memberIndex, member.bytesTaken)
          (newState, loc)
      }
      def allocStruct(struct: Struct): S[PackedStruct] =
        struct.members
          .zipWithIndex
          .sortBy(-_._1.size) // sort descending by member size
          .traverse { case (s, i) => alloc(s).map((_, i)) }
          .map(ms => PackedStruct(ms.sortBy(_._2).map(_._1)))

      def alloc(tpe: Type): S[PackedLoc] = tpe match {
        case s: Struct => allocStruct(s).widen
        case t => allocAtom(t.size).widen
      }

      val (init, tag) = alloc(tagType).run(AllocState(repr.members.map(m => AllocMember(m.size, 0)))).value
      val variantMappings = variants.map(allocStruct(_).runA(init).value)
      (variantMappings.toVector, tag)
    }

  }

  implicit val show: Show[Type] = new Show[Type] {
    def show(t: Type): String = t match {
      case Fun(params, ret) => show"(${params.map(show).mkString(", ")}) => ${show(ret)}"
      case Struct(members) => members.map(show).mkString("{", ", ", "}")
      case Enum(variants) => variants.map(show).mkString("(", "|", ")")
      case t => t.toString.toLowerCase
    }
  }

}
