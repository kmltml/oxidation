package oxidation
package codegen.pass

import cats._
import cats.data._
import cats.implicits._

import ir._

object UnitRemoval extends IdPass {

  override def name = "unit-removal"

  override val onVal = {
    case Val(_, Type.U0) => Val.I(0, Type.U0)
  }

  override val onInstruction = {
    case Inst.Move(Register(_, _, Type.U0), op) => Vector(Inst.Do(op))
  }

}
