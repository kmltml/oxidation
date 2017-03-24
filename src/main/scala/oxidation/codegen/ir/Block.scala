package oxidation
package codegen
package ir

case class Block(name: Name, instructions: Vector[Inst], flow: FlowControl) {

  def reads: Set[Register] = instructions.flatMap {
    case Inst.Label(_) => Vector.empty
    case Inst.Eval(_, op) => op.reads.toVector
    case Inst.Flow(f) => f.reads.toVector
  }.toSet ++ flow.reads

}
