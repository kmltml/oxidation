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
    case Inst.Move(dest, op @ Op.Convert(_, Type.F(_))) =>
      spill(dest).map { case (d, insts) => Inst.Move(d, op) +: insts }
    case Inst.Move(dest, Op.Binary(op @ (InfixOp.Add | InfixOp.Sub | InfixOp.Mul | InfixOp.Div), left, right)) =>
      for {
        d <- spill(dest)
      } yield Inst.Move(d._1, Op.Binary(op, left, right)) +: d._2
    case Inst.Move(dest, Op.Binary(op @ (InfixOp.Gt | InfixOp.Lt | InfixOp.Leq | InfixOp.Geq), l, r)) =>
      fill(l).map { case (insts, left) => insts :+ Inst.Move(dest, Op.Binary(op, left, r)) }
  }

  override def onInstruction = extraOnInstruction orElse super.onInstruction

}
