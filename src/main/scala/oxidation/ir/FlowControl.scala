package oxidation
package ir

import oxidation.codegen.Name

import cats._
import cats.data._
import cats.implicits._

sealed trait FlowControl {

  import FlowControl._

  def reads: Set[Register] =
    vals.flatMap(_.reads)

  def vals: Set[Val] = this match {
    case Branch(v, _, _) => Set(v)
    case Return(v) => Set(v)
    case Goto(_) | Unreachable => Set.empty
  }

}

object FlowControl {

  final case class Return(value: Val) extends FlowControl
  final case class Goto(target: Name) extends FlowControl
  final case class Branch(cond: Val, ifTrue: Name, ifFalse: Name) extends FlowControl
  final case object Unreachable extends FlowControl

  implicit def show: Show[FlowControl] = {
    case Return(v) => show"return $v"
    case Goto(t) => show"goto $t"
    case Branch(c, t, f) => show"branch $c, $t, $f"
    case Unreachable => "unreachable"
  }

}
