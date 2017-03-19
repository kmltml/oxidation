package oxidation
package backend
package amd64

sealed trait Reg extends Product with Serializable {

  def *(i: Int): (Reg, Int) = (this, i) // to allow for mov(Val.m(Reg.RBX * 2, 3), Val.I(10)) syntax

}

object Reg {

  case object RAX extends Reg
  case object RBX extends Reg
  case object RCX extends Reg
  case object RDX extends Reg

  case object RSP extends Reg
  case object RBP extends Reg
  case object RSI extends Reg
  case object RDI extends Reg

  case object R8 extends Reg
  case object R9 extends Reg
  case object R10 extends Reg
  case object R11 extends Reg
  case object R12 extends Reg
  case object R13 extends Reg
  case object R14 extends Reg
  case object R15 extends Reg

}
