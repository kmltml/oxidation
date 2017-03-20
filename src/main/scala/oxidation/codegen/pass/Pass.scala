package oxidation
package codegen
package pass

trait Pass {

  type =?>[A, B] = PartialFunction[A, B]

  def onDef: ir.Def =?> Vector[ir.Def] = PartialFunction.empty

  def onBlock: ir.Block =?> Vector[ir.Block] = PartialFunction.empty

  def onInstruction: ir.Inst =?> Vector[ir.Inst] = PartialFunction.empty

  def onFlow: ir.FlowControl =?> ir.FlowControl = PartialFunction.empty

  def txBlock(block: ir.Block): Vector[ir.Block] = {
    val blocks = onBlock.lift(block).getOrElse(Vector(block))
    blocks.map {
      case ir.Block(name, instrs, flow) =>
        val newInstrs = instrs.flatMap(i => onInstruction.lift(i).getOrElse(Vector(i)))
        val newFlow = onFlow.lift(flow).getOrElse(flow)
        ir.Block(name, newInstrs, newFlow)
    }
  }

  def txDef(d: ir.Def): Vector[ir.Def] = {
    val defs = onDef.lift(d).getOrElse(Vector(d))
    defs.map {
      case ir.Def.Fun(name, params, ret, body) =>
        ir.Def.Fun(name, params, ret, body.flatMap(txBlock))
    }
  }

}
