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

  final case class Ptr(pointee: Type) extends Type

  final case class Arr(member: Type, size: Int) extends Type

  final case class Fun(params: List[Type], ret: Type) extends Type

  final class Struct(val name: Symbol, membersF: Struct => List[StructMember]) extends Type {

    val members = membersF(this)

    def indexOf(memberName: String): Int =
      members.indexWhere(_.name == memberName)

    override def equals(obj: Any): Boolean = obj match {
      case s: Struct => (this eq s) || (name == s.name)
      case _ => false
    }

    override def toString: String = s"Struct($name)"

  }

  object Struct {

    def apply(name: Symbol, members: StructMember*): Struct = new Struct(name, _ => members.toList)

    def apply(name: Symbol)(members: Struct => List[StructMember]): Struct = new Struct(name, members)

    def unapply(s: Struct): Option[(Symbol, List[StructMember])] = Some((s.name, s.members))

  }

  final case class StructMember(name: String, typ: Type)

  final class Enum(val name: Symbol, variantsF: Enum => List[EnumVariant]) extends Type {

    val variants = variantsF(this)

    override def equals(obj: Any): Boolean = obj match {
      case e: Enum => (this eq e) || (e.name == name)
      case _ => false
    }

    override def toString: String = s"Enum($name)"

  }

  object Enum {

    def apply(name: Symbol, variants: List[EnumVariant]): Enum = new Enum(name, _ => variants)

    def apply(name: Symbol)(f: Enum => List[EnumVariant]): Enum = new Enum(name, f)

    def unapply(e: Enum): Option[(Symbol, List[EnumVariant])] = Some((e.name, e.variants))

  }

  final case class EnumVariant(name: Symbol, members: List[StructMember]) extends Type

}
