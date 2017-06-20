package oxidation
package backend
package amd64

sealed abstract class ConditionCode(val repr: String) {

  import ConditionCode._

  def unary_! : ConditionCode = this match {
    case Overflow => NotOverflow
    case NotOverflow => Overflow
    case Below => NotBelow
    case NotBelow => Below
    case Zero => NotZero
    case NotZero => Zero
    case NotAbove => Above
    case Above => NotAbove
    case Sign => NotSign
    case NotSign => Sign
    case Parity => NotParity
    case NotParity => Parity
    case Less => NotLess
    case NotLess => Less
    case NotGreater => Greater
    case Greater => NotGreater
  }

}

object ConditionCode {

  object Overflow extends ConditionCode("o")
  object NotOverflow extends ConditionCode("no")

  object Below extends ConditionCode("b")
  val Carry = Below
  val NotAboveOrEqual = Below

  object NotBelow extends ConditionCode("nb")
  val NotCarry = NotBelow
  val AboveOrEqual = NotBelow

  object Zero extends ConditionCode("z")
  val Equal = Zero

  object NotZero extends ConditionCode("nz")
  val NotEqual = NotZero

  object NotAbove extends ConditionCode("na")
  val BelowOrEqual = NotAbove

  object Above extends ConditionCode("a")
  val NotBelowOrEqual = Above

  object Sign extends ConditionCode("s")
  object NotSign extends ConditionCode("ns")

  object Parity extends ConditionCode("p")
  val ParityEven = Parity

  object NotParity extends ConditionCode("np")
  val ParityOdd = NotParity

  object Less extends ConditionCode("l")
  val NotGreaterOrEqual = Less

  object NotLess extends ConditionCode("nl")
  val GreaterOrEqual = NotLess

  object NotGreater extends ConditionCode("ng")
  val LessOrEqual = NotGreater

  object Greater extends ConditionCode("g")
  val NotLessOrEqual = Less


}
