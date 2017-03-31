package oxidation
package codegen
package pass

object ExplicitBlocks extends IdPass {

  def name = "explicit-blocks"

  override def onBlock: ir.Block =?> Vector[ir.Block] = {
    case ir.Block(name, instructions, flow) =>
      divide(name, instructions, flow) ensuring { blocks => blocks.forall(_.instructions.forall {
        case ir.Inst.Move(_, _) | ir.Inst.Do(_) => true
        case ir.Inst.Flow(_) | ir.Inst.Label(_) => false
      })}
  }

  private def findPrefix(insts: Vector[ir.Inst]): (Vector[ir.Inst], Vector[ir.Inst]) =
    insts.span {
      case _: ir.Inst.Flow | _: ir.Inst.Label => false
      case _ => true
    }

  private def divide(name: Name, insts: Vector[ir.Inst], lastFlow: ir.FlowControl): Vector[ir.Block] = {
    findPrefix(insts) match {
      case (body, ir.Inst.Label(nextName) +: tail) =>
        ir.Block(name, body, ir.FlowControl.Goto(nextName)) +: divide(nextName, tail, lastFlow)
      case (body, ir.Inst.Flow(flow) +: ir.Inst.Label(nextName) +: tail) =>
        ir.Block(name, body, flow) +: divide(nextName, tail, lastFlow)
      case (body, Vector()) =>
        Vector(ir.Block(name, body, lastFlow))
    }
  }

}
