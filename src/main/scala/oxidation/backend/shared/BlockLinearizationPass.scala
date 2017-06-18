package oxidation
package backend
package shared

import codegen.pass.IdPass
import ir._

object BlockLinearizationPass extends IdPass {

  override def name = "block-linearization"

  override val onDef = {
    case fun @ Def.Fun(_, _, _, blocks, _) =>
      val flattened = blocks.flatMap {
        case Block(lbl, insts, flow) =>
          Inst.Label(lbl) +: insts :+ Inst.Flow(flow)
      }
      val name = blocks.head.name
      val flow = blocks.last.flow

      val transformed = flattened.tails.flatMap(onInsts)

      Vector(fun.copy(body = Vector(Block(name, transformed.toVector, flow))))
  }

  private def onInsts(insts: Vector[Inst]): Vector[Inst] = insts match {
    case Inst.Flow(FlowControl.Goto(target)) +:
         Inst.Label(lbl) +: _
      if target == lbl => Vector.empty

    case i +: _ => Vector(i)

    case Vector() => Vector.empty

  }

}
