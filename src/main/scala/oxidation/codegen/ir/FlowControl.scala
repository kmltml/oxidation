package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait FlowControl {

  def reads: Set[ir.Register] = {
    val vals = this match {
      case ir.FlowControl.Branch(v, _, _) => Set(v)
      case ir.FlowControl.Return(v) => Set(v)
      case ir.FlowControl.Goto(_) => Set.empty
    }
    vals.collect {
      case ir.Val.R(r) => r
    }
  }

}

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
