package oxidation
package codegen
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait Inst

object Inst {

  final case class Eval(dest: Option[Register], op: Op) extends Inst
  final case class Label(name: Name) extends Inst

  implicit val show: Show[Inst] = {
    case Eval(None, op) => op.show
    case Eval(Some(reg), op) => show"$reg = $op"

    case Label(name) => show"$name:"
  }

}
