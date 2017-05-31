package oxidation
package backend
package amd64

import ir._

object IntSpillPass extends RegisterSpillPass {

  private def extraOnInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Move(dest @ Register(_, _, _: Type.F), conv @ Op.Convert(_, _: Type.I)) =>
      spill(dest).map { case (d, insts) => Inst.Move(d, conv) +: insts }
  }

  override def onInstruction = extraOnInstruction orElse super.onInstruction

}
