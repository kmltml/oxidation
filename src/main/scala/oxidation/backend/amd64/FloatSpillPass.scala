package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir._

object FloatSpillPass extends RegisterSpillPass {

  private val extraOnInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Move(dest, Op.Copy(src @ (ir.Val.F32(_) | ir.Val.F64(_)))) =>
      for {
        d <- spill(dest)
      } yield Inst.Move(d._1, Op.Copy(src)) +: d._2
    case Inst.Move(dest, Op.Sqrt(src)) =>
      for {
        d <- spill(dest)
      } yield Inst.Move(d._1, Op.Sqrt(src)) +: d._2
  }

  override def onInstruction = extraOnInstruction orElse super.onInstruction

}
