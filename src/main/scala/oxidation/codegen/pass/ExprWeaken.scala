package oxidation
package codegen
package pass

import ir._

object ExprWeaken extends IdPass {

  override def name = "expr-weaken"

  override def onInstruction = {
    // x * 0 == 0 * x == 0
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, _, Val.I(0, _))) =>
      Vector(Inst.Move(dest, Op.Copy(Val.I(0, dest.typ))))
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, Val.I(0, _), _)) =>
      Vector(Inst.Move(dest, Op.Copy(Val.I(0, dest.typ))))

    // x * 1 == 1 * x == x
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, v, Val.I(1, _))) =>
      Vector(Inst.Move(dest, Op.Copy(v)))
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, Val.I(1, _), v)) =>
      Vector(Inst.Move(dest, Op.Copy(v)))

    // x + 0 == 0 + x == x
    case Inst.Move(dest, Op.Binary(InfixOp.Add, Val.I(0, _), v)) =>
      Vector(Inst.Move(dest, Op.Copy(v)))
    case Inst.Move(dest, Op.Binary(InfixOp.Add, v, Val.I(0, _))) =>
      Vector(Inst.Move(dest, Op.Copy(v)))

    // x * (2|4|8|...) == (2|4|8|...) * x == x << (1|2|3|...)
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, v, Val.I(i, _)))
      if Integer.bitCount(i) == 1 =>
      Vector(Inst.Move(dest, Op.Binary(InfixOp.Shl, v, Val.I(Integer.numberOfTrailingZeros(i), dest.typ))))
    case Inst.Move(dest, Op.Binary(InfixOp.Mul, Val.I(i, _), v))
      if Integer.bitCount(i) == 1 =>
      Vector(Inst.Move(dest, Op.Binary(InfixOp.Shl, v, Val.I(Integer.numberOfTrailingZeros(i), dest.typ))))

    // x / 1 == x
    case Inst.Move(dest, Op.Binary(InfixOp.Div, v, Val.I(1, _))) =>
      Vector(Inst.Move(dest, Op.Copy(v)))

    // x / (2|4|8|...) = x >> (1|2|3|...)
    case Inst.Move(dest, Op.Binary(InfixOp.Div, v, Val.I(i, _)))
      if Integer.bitCount(i) == 1 =>
      Vector(Inst.Move(dest, Op.Binary(InfixOp.Shr, v, Val.I(Integer.numberOfTrailingZeros(i), dest.typ))))

  }

}
