package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait FlowControl

object FlowControl {

  final case class Return(value: Val) extends FlowControl
  final case class Goto(target: Name) extends FlowControl
  final case class Branch(cond: Val, ifTrue: Name, ifFalse: Name) extends FlowControl

  implicit def show: Show[FlowControl] = {
    case Return(v) => show"return $v"
    case Goto(t) => show"goto $t"
    case Branch(c, t, f) => show"branch $c, $t, $f"
  }

}
