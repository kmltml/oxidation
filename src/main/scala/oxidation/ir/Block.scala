package oxidation
package ir

import codegen.Name

case class Block(name: Name, instructions: Vector[Inst], flow: FlowControl) {

  def reads: Set[Register] = instructions.flatMap {
    case Inst.Label(_) => Vector.empty
    case Inst.Move(_, op) => op.reads.toVector
    case Inst.Do(op) => op.reads.toVector
    case Inst.Flow(f) => f.reads.toVector
  }.toSet ++ flow.reads

  def writes: Set[Register] = instructions.flatMap {
    case Inst.Move(dest, _) => Vector(dest)
    case _ => Vector.empty
  }.toSet

}
