package oxidation.backend.amd64

import oxidation.codegen.Name

sealed trait Val extends Product with Serializable

object Val {

  final case class R(reg: Reg) extends Val
  final case class I(int: Int) extends Val
  final case class L(name: Name) extends Val
  final case class M(size: RegSize, regs: List[(Reg, Int)], offset: Int) extends Val

  def m(size: RegSize, offsets: MemOffset*): Val.M = offsets.foldLeft(M(size, List.empty, 0)) {
    case (v, MemOffset.MultipliedReg(r)) => v.copy(regs = v.regs :+ r)
    case (v, MemOffset.Offset(i)) => v.copy(offset = v.offset + i)
  }
  implicit def r(reg: Reg): Val = R(reg)
  implicit def i(int: Int): Val = I(int)

  sealed trait MemOffset
  object MemOffset {
    final case class MultipliedReg(value: (Reg, Int)) extends MemOffset
    final case class Offset(value: Int) extends MemOffset

    implicit def reg(r: Reg): MultipliedReg = MultipliedReg(r * 1)
    implicit def mreg(value: (Reg, Int)): MultipliedReg = MultipliedReg(value)
    implicit def off(i: Int): Offset = Offset(i)
    implicit def fromVal(v: Val): MemOffset = v match {
      case R(r) => reg(r)
      case I(i) => off(i)
    }
  }

}
