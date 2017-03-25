package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Inst {

  import Inst._

  def regs: Set[Register] = this match {
    case Move(dest, op) => op.reads + dest
    case Do(op) => op.reads
    case Label(_) => Set.empty
    case Flow(f) => f.reads
  }

}

object Inst {

  final case class Move(dest: Register, op: Op) extends Inst
  final case class Do(op: Op) extends Inst
  final case class Label(name: Name) extends Inst
  final case class Flow(flowControl: FlowControl) extends Inst

  implicit val show: Show[Inst] = {
    case Do(op) => op.show
    case Move(reg, op) => show"$reg = $op"

    case Label(name) => show"$name:"

    case Flow(f) => f.show
  }

}
