package oxidation
package codegen.pass

import ir._

object ArrInit extends IdPass {

  override def name = "arr-init"

  override def onInstruction = {
    case Inst.Move(dest, Op.Copy(arr @ Val.Array(elems))) =>
      Inst.Move(dest, Op.Copy(Val.UArr(arr.typ))) +:
        elems.toVector.zipWithIndex.map {
          case (v, i) => Inst.Do(Op.ArrStore(Val.R(dest), Val.I(i, Type.I64), v))
        }
  }

}
