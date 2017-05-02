package oxidation
package analyze

sealed trait Type

object Type {

  sealed trait Num extends Type

  sealed trait Integral extends Num

  sealed abstract class I(val bits: Int) extends Integral
  object I {
    def unapply(arg: I): Option[Int] = Some(arg.bits)
  }

  case object I8 extends I(8)
  case object I16 extends I(16)
  case object I32 extends I(32)
  case object I64 extends I(64)

  sealed abstract class U(val bits: Int) extends Integral
  object U {
    def unapply(arg: U): Option[Int] = Some(arg.bits)
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

  case object U1 extends Type
  case object U0 extends Type

  // pointee is an unresolved type, to allow for self-referencing structs (eg. `enum List[A] = { Cons { head: A; tail: ptr[List[A]] }; Nil }`)
  final case class Ptr(pointee: TypeName) extends Type

  final case class Arr(member: Type, size: Int) extends Type

  final case class Fun(params: List[Type], ret: Type) extends Type

  final case class Struct(name: Symbol, members: List[StructMember]) extends Type {

    def indexOf(memberName: String): Int =
      members.indexWhere(_.name == memberName)

  }

  final case class StructMember(name: String, typ: Type)

}
