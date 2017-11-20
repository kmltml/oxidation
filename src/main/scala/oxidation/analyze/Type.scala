package oxidation
package analyze

sealed trait Type {

  import Type._

  def symbol: Symbol

  def subst(s: Map[String, Type]): Type = this match {
    case Var(n) => s.get(n) match {
      case None => Var(n)
      case Some(t) => t
    }
    case (_: Num | U1 | U0) => this
    case Ptr(pointee) => Ptr(pointee.subst(s))
    case Arr(member, size) => Arr(member.subst(s), size)
    case Fun(params, ret) => Fun(params.map(_.subst(s)), ret.subst(s))
    case FunPtr(params, ret) => FunPtr(params.map(_.subst(s)), ret.subst(s))
    case Struct(name, members) => Struct(name, members.map(_.subst(s)): _*) // TODO handle recursive struct definitions somehow
    case Enum(name, variants) => Enum(name, variants.map(_.subst(s)))
    case EnumConstructor(enum, variant) => ???
    case TypeLambda(name, paramNames, body) =>
      body.subst(s -- paramNames)
  }

  def rename(s: Symbol): Type = this match {
    case Struct(_, ms) => Struct(s, ms:_*)
    case TypeLambda(_, ps, b) => TypeLambda(s, ps, b)
    case Enum(_, vs) => Enum(s, vs) // TODO rename the variants too?
    case _ => this // TODO is it safe?
  }

}

object Type {

  final case class Var(name: String) extends Type {
    def symbol = Symbol.Local(name)
  }

  sealed trait ValueType extends Type

  sealed trait Num extends ValueType

  sealed trait Integral extends Num

  sealed abstract class I(val bits: Int) extends Integral {
    def symbol = Symbol.Global(s"i$bits" :: Nil)
  }
  object I {
    def unapply(arg: I): Option[Int] = Some(arg.bits)
  }

  case object I8 extends I(8)
  case object I16 extends I(16)
  case object I32 extends I(32)
  case object I64 extends I(64)

  sealed abstract class U(val bits: Int) extends Integral {
    def symbol = Symbol.Global(s"u$bits" :: Nil)
  }
  object U {
    def unapply(arg: U): Option[Int] = Some(arg.bits)
  }

  case object U8 extends U(8)
  case object U16 extends U(16)
  case object U32 extends U(32)
  case object U64 extends U(64)

  sealed abstract class F(val bits: Int) extends Num {
    def symbol = Symbol.Global(s"f$bits" :: Nil)
  }
  object F {
    def unapply(arg: F): Option[Int] = Some(arg.bits)
  }

  case object F32 extends F(32)
  case object F64 extends F(64)

  case object U1 extends ValueType {
    def symbol = Symbol.Global("u1" :: Nil)
  }
  case object U0 extends ValueType {
    def symbol = Symbol.Global("u0" :: Nil)
  }

  final case class Ptr(pointee: Type) extends ValueType {
    def symbol = Symbol.Specialized(List(pointee.symbol), Symbol.Global("ptr" :: Nil))
  }

  final case class Arr(member: Type, size: Int) extends ValueType {
    def symbol = Symbol.Specialized(List(member.symbol, Symbol.Global(size.toString :: Nil)), Symbol.Global("arr" :: Nil))
  }

  final case class Fun(params: List[Type], ret: Type) extends Type {

    def ptr: FunPtr = FunPtr(params, ret)
    def symbol = Symbol.Specialized(params.map(_.symbol) :+ ret.symbol, Symbol.Global("fun" :: Nil))

  }

  final case class FunPtr(params: List[Type], ret: Type) extends ValueType {
    def symbol = Symbol.Specialized(params.map(_.symbol) :+ ret.symbol, Symbol.Global("funptr" :: Nil))
  }

  final class Struct(val name: Symbol, membersF: Struct => List[StructMember]) extends ValueType {

    val members = membersF(this)
    def symbol = name

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

  final case class StructMember(name: String, typ: Type) {

    def subst(s: Map[String, Type]): StructMember =
      StructMember(name, typ.subst(s))

  }

  final class Enum(val name: Symbol, variantsF: Enum => List[EnumVariant]) extends ValueType {

    val variants = variantsF(this)
    def symbol = name

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

  final case class EnumVariant(name: Symbol, members: List[StructMember]) {
    def symbol = name

    def subst(s: Map[String, Type]): EnumVariant =
      EnumVariant(name, members.map(_.subst(s)))
  }

  final case class EnumConstructor(enumType: Enum, variant: EnumVariant) extends Type {
    def symbol = variant.name
  }

  final case class TypeLambda(name: Symbol, paramNames: List[String], body: Type) extends Type {
    def symbol = name
    def apply(params: List[Type]): Type =
      body.subst((paramNames zip params).toMap)
        .rename(Symbol.Specialized(params.map(_.symbol), name))
  }

}
