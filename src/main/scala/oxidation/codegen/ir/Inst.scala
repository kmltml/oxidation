package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Inst {

  import Inst._

  def regs: Set[Register] = this match {
    case Eval(dest, op) => op.reads ++ dest
    case Label(_) => Set.empty
    case Flow(f) => f.reads
  }

}

object Inst {

  final case class Eval(dest: Option[Register], op: Op) extends Inst
  final case class Label(name: Name) extends Inst
  final case class Flow(flowControl: FlowControl) extends Inst

  implicit val show: Show[Inst] = {
    case Eval(None, op) => op.show
    case Eval(Some(reg), op) => show"$reg = $op"

    case Label(name) => show"$name:"

    case Flow(f) => f.show
  }

}
