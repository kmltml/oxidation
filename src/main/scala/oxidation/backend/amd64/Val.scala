package oxidation.backend.amd64

import oxidation.codegen.Name

sealed trait Val extends Product with Serializable

object Val {

  final case class R(reg: Reg) extends Val
  final case class I(int: Long) extends Val
  final case class L(name: Name) extends Val
  final case class M(size: Option[RegSize], regs: List[(Reg, Long)], offset: Long, labels: List[Name]) extends Val {

    def +(m: M): M =
      M(size orElse m.size, regs ++ m.regs, offset + m.offset, labels ++ m.labels)

  }

  def m(size: Option[RegSize], offsets: MemOffset*): Val.M = offsets.foldLeft(M(size, List.empty, 0, List.empty)) {
    case (v, MemOffset.MultipliedReg(r)) => v.copy(regs = v.regs :+ r)
    case (v, MemOffset.Offset(i)) => v.copy(offset = v.offset + i)
    case (v, MemOffset.Label(n)) => v.copy(labels = n :: v.labels)
  }
  implicit def r(reg: Reg): Val = R(reg)
  implicit def i(int: Long): Val = I(int)

  sealed trait MemOffset
  object MemOffset {
    final case class MultipliedReg(value: (Reg, Long)) extends MemOffset
    final case class Offset(value: Long) extends MemOffset
    final case class Label(name: Name) extends MemOffset

    implicit def reg(r: Reg): MultipliedReg = MultipliedReg(r * 1)
    implicit def mreg(value: (Reg, Long)): MultipliedReg = MultipliedReg(value)
    implicit def off(i: Long): Offset = Offset(i)
    implicit def lbl(l: Name): Label = Label(l)
    implicit def fromVal(v: Val): MemOffset = v match {
      case R(r) => reg(r)
      case I(i) => off(i)
      case L(n) => lbl(n)
    }
  }

}
