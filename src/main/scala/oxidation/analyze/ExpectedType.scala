package oxidation.analyze

sealed trait ExpectedType extends Product with Serializable

object ExpectedType {

  case object Undefined extends ExpectedType
  case object Appliable extends ExpectedType
  case class Numeric(supertypeOf: Option[Type]) extends ExpectedType
  case class Specific(typ: Type) extends ExpectedType

}
