package oxidation
package analyze

sealed trait Type

object Type {

  sealed trait Num extends Type

  case object I8 extends Num
  case object I16 extends Num
  case object I32 extends Num
  case object I64 extends Num
  case object U8 extends Num
  case object U16 extends Num
  case object U32 extends Num
  case object U64 extends Num

  case object U1 extends Type
  case object U0 extends Type

  // pointee is an unresolved type, to allow for self-referencing structs (eg. `enum List[A] = { Cons { head: A; tail: ptr[List[A]] }; Nil }`)
  final case class Ptr(pointee: TypeName) extends Type

  final case class Fun(params: List[Type], ret: Type) extends Type

  final case class Struct(name: Symbol, members: List[StructMember]) extends Type {

    def indexOf(memberName: String): Int =
      members.indexWhere(_.name == memberName)

  }

  final case class StructMember(name: String, typ: Type)

}
