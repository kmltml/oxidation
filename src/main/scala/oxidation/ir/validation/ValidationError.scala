package oxidation
package ir
package validation

sealed trait ValidationError extends Product with Serializable

object ValidationError {

  final case class UseBeforeDefine(location: Location, reg: Register) extends ValidationError
  final case class WrongType(location: Location, expected: Type, found: Type) extends ValidationError
  final case class NotANumericType(location: Location, found: Type) extends ValidationError
  final case class NotAFunction(location: Location, found: Type) extends ValidationError
  final case class NotAStruct(location: Location, found: Type) extends ValidationError
  final case class WrongArity(location: Location, expected: Int, found: Int) extends ValidationError
  final case class StructMemberOutOfBounds(location: Location, typ: Type.Struct, index: Int) extends ValidationError

}
