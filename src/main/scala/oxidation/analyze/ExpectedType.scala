package oxidation.analyze

/**
  * Created by Kamil on 24.02.2017.
  */
sealed trait ExpectedType

object ExpectedType {

  case object Undefined extends ExpectedType
  case object Numeric extends ExpectedType
  case object Appliable extends ExpectedType
  case class Specific(typ: Type) extends ExpectedType

}
