package oxidation
package codegen

import cats._
import cats.data._
import cats.implicits._

case class CodegenState(nextReg: Int = 0, registerBindings: Map[Symbol, ir.Register] = Map.empty)

object CodegenState {

  def genReg: State[CodegenState, ir.Register] = State {
    case s @ CodegenState(nextReg, _) =>
      (s.copy(nextReg = nextReg + 1), ir.Register(nextReg))
  }

  def withBindings(bindings: (Symbol, ir.Register)*): State[CodegenState, Unit] =
    State.modify(s => s.copy(registerBindings = s.registerBindings ++ bindings))

}
