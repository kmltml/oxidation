package oxidation
package ir

import codegen.Name

case class Block(name: Name, instructions: Vector[Inst], flow: FlowControl) {

  def reads: Set[Register] =
    instructions.flatMap(_.reads).toSet ++ flow.reads

  def writes: Set[Register] = instructions.flatMap {
    case Inst.Move(dest, _) => Vector(dest)
    case _ => Vector.empty
  }.toSet

}
