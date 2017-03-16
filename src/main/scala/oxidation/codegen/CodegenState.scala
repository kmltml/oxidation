package oxidation
package codegen

import cats._
import cats.data._
import cats.implicits._

case class CodegenState(nextReg: Int = 0, nextName: Int = 0, registerBindings: Map[Symbol, ir.Register] = Map.empty)

object CodegenState {

  def genReg(t: ir.Type): State[CodegenState, ir.Register] = State { s =>
    (s.copy(nextReg = s.nextReg + 1), ir.Register(s.nextReg, t))
  }

  def genLocalName(prefix: String): State[CodegenState, Name] = State { s =>
    (s.copy(nextName = s.nextName + 1), Name.Local(prefix, s.nextName))
  }

  def withBindings(bindings: (Symbol, ir.Register)*): State[CodegenState, Unit] =
    State.modify(s => s.copy(registerBindings = s.registerBindings ++ bindings))

  def inspect[A](f: CodegenState => A): State[CodegenState, A] =
    State.inspect(f)

}
