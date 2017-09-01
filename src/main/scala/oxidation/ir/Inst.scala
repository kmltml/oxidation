package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

sealed trait Inst {

  import Inst._

  def regs: Set[Register] = this match {
    case Move(dest, op) => op.reads + dest
    case Do(op) => op.reads
    case Label(_) => Set.empty
    case Flow(f) => f.reads
  }

  def vals: Set[Val] = this match {
    case Move(_, op) => op.vals
    case Do(op) => op.vals
    case Label(_) => Set.empty
    case Flow(f) => f.vals
  }

  def reads: Set[Register] = vals.flatMap(_.reads)

}

object Inst {

  final case class Move(dest: Register, op: Op) extends Inst
  final case class Do(op: Op) extends Inst
  final case class Label(name: Name) extends Inst
  final case class Flow(flowControl: FlowControl) extends Inst

  object Eval {

    def apply(dest: Option[Register], op: Op): Inst = dest match {
      case None => Do(op)
      case Some(r) => Move(r, op)
    }

    def unapply(i: Inst): Option[(Option[Register], Op)] = i match {
      case Move(r, o) => Some((Some(r), o))
      case Do(o) => Some((None, o))
      case _ => None
    }

  }

  implicit val show: Show[Inst] = {
    case Do(op) => op.show
    case Move(reg, op) => show"$reg = $op"

    case Label(name) => show"$name:"

    case Flow(f) => f.show
  }

}
