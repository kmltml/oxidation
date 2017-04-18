package oxidation
package backend
package amd64

final case class Reg(loc: RegLoc, size: RegSize) {

  def *(i: Long): (Reg, Long) = (this, i) // to allow for mov(Val.m(Reg.RBX * 2, 3), Val.I(10)) syntax

  override def toString: String = {
    def prefix(s: RegSize) = s match {
      case RegSize.Byte | RegSize.Word => ""
      case RegSize.DWord => "e"
      case RegSize.QWord => "r"
    }
    def suffix(s: RegSize) = s match {
      case RegSize.Byte => "l"
      case _ => "x"
    }
    def newSuffix(s: RegSize) = s match {
      case RegSize.Byte => "b"
      case RegSize.Word => "w"
      case RegSize.DWord => "d"
      case RegSize.QWord => ""
    }
    def extraSuffix(s: RegSize) = s match {
      case RegSize.Byte => "l"
      case RegSize.Word | RegSize.DWord | RegSize.QWord => ""
    }
    loc match {
      case RegLoc.A => prefix(size) + "a" + suffix(size)
      case RegLoc.B => prefix(size) + "b" + suffix(size)
      case RegLoc.C => prefix(size) + "c" + suffix(size)
      case RegLoc.D => prefix(size) + "d" + suffix(size)
      case RegLoc.SP => prefix(size) + "sp" + extraSuffix(size)
      case RegLoc.BP => prefix(size) + "bp" + extraSuffix(size)
      case RegLoc.SI => prefix(size) + "si" + extraSuffix(size)
      case RegLoc.DI => prefix(size) + "di" + extraSuffix(size)
      case RegLoc.R8 => "r8" + newSuffix(size)
      case RegLoc.R9 => "r9" + newSuffix(size)
      case RegLoc.R10 => "r10" + newSuffix(size)
      case RegLoc.R11 => "r11" + newSuffix(size)
      case RegLoc.R12 => "r12" + newSuffix(size)
      case RegLoc.R13 => "r13" + newSuffix(size)
      case RegLoc.R14 => "r14" + newSuffix(size)
      case RegLoc.R15 => "r15" + newSuffix(size)
    }
  }

}

object Reg {

  final val AH = Reg(RegLoc.A, RegSize.Byte)
  final val BH = Reg(RegLoc.B, RegSize.Byte)
  final val CH = Reg(RegLoc.C, RegSize.Byte)
  final val DH = Reg(RegLoc.D, RegSize.Byte)
  final val R8B = Reg(RegLoc.R8, RegSize.Byte)
  final val R9B = Reg(RegLoc.R9, RegSize.Byte)
  final val R10B = Reg(RegLoc.R10, RegSize.Byte)
  final val R11B = Reg(RegLoc.R11, RegSize.Byte)
  final val R12B = Reg(RegLoc.R12, RegSize.Byte)
  final val R13B = Reg(RegLoc.R13, RegSize.Byte)
  final val R14B = Reg(RegLoc.R14, RegSize.Byte)
  final val R15B = Reg(RegLoc.R15, RegSize.Byte)

  final val AX = Reg(RegLoc.A, RegSize.Word)
  final val BX = Reg(RegLoc.B, RegSize.Word)
  final val CX = Reg(RegLoc.C, RegSize.Word)
  final val DX = Reg(RegLoc.D, RegSize.Word)
  final val BP = Reg(RegLoc.BP, RegSize.Word)
  final val SP = Reg(RegLoc.SP, RegSize.Word)
  final val SI = Reg(RegLoc.SI, RegSize.Word)
  final val DI = Reg(RegLoc.DI, RegSize.Word)
  final val R8W = Reg(RegLoc.R8, RegSize.Word)
  final val R9W = Reg(RegLoc.R9, RegSize.Word)
  final val R10W = Reg(RegLoc.R10, RegSize.Word)
  final val R11W = Reg(RegLoc.R11, RegSize.Word)
  final val R12W = Reg(RegLoc.R12, RegSize.Word)
  final val R13W = Reg(RegLoc.R13, RegSize.Word)
  final val R14W = Reg(RegLoc.R14, RegSize.Word)
  final val R15W = Reg(RegLoc.R15, RegSize.Word)

  final val EAX = Reg(RegLoc.A, RegSize.DWord)
  final val EBX = Reg(RegLoc.B, RegSize.DWord)
  final val ECX = Reg(RegLoc.C, RegSize.DWord)
  final val EDX = Reg(RegLoc.D, RegSize.DWord)
  final val EBP = Reg(RegLoc.BP, RegSize.DWord)
  final val ESP = Reg(RegLoc.SP, RegSize.DWord)
  final val ESI = Reg(RegLoc.SI, RegSize.DWord)
  final val EDI = Reg(RegLoc.DI, RegSize.DWord)
  final val R8D = Reg(RegLoc.R8, RegSize.DWord)
  final val R9D = Reg(RegLoc.R9, RegSize.DWord)
  final val R10D = Reg(RegLoc.R10, RegSize.DWord)
  final val R11D = Reg(RegLoc.R11, RegSize.DWord)
  final val R12D = Reg(RegLoc.R12, RegSize.DWord)
  final val R13D = Reg(RegLoc.R13, RegSize.DWord)
  final val R14D = Reg(RegLoc.R14, RegSize.DWord)
  final val R15D = Reg(RegLoc.R15, RegSize.DWord)

  final val RAX = Reg(RegLoc.A, RegSize.QWord)
  final val RBX = Reg(RegLoc.B, RegSize.QWord)
  final val RCX = Reg(RegLoc.C, RegSize.QWord)
  final val RDX = Reg(RegLoc.D, RegSize.QWord)
  final val RBP = Reg(RegLoc.BP, RegSize.QWord)
  final val RSP = Reg(RegLoc.SP, RegSize.QWord)
  final val RSI = Reg(RegLoc.SI, RegSize.QWord)
  final val RDI = Reg(RegLoc.DI, RegSize.QWord)
  final val R8 = Reg(RegLoc.R8, RegSize.QWord)
  final val R9 = Reg(RegLoc.R9, RegSize.QWord)
  final val R10 = Reg(RegLoc.R10, RegSize.QWord)
  final val R11 = Reg(RegLoc.R11, RegSize.QWord)
  final val R12 = Reg(RegLoc.R12, RegSize.QWord)
  final val R13 = Reg(RegLoc.R13, RegSize.QWord)
  final val R14 = Reg(RegLoc.R14, RegSize.QWord)
  final val R15 = Reg(RegLoc.R15, RegSize.QWord)

}

sealed trait RegLoc extends Product with Serializable

object RegLoc {
  case object A extends RegLoc
  case object B extends RegLoc
  case object C extends RegLoc
  case object D extends RegLoc
  case object SI extends RegLoc
  case object DI extends RegLoc
  case object SP extends RegLoc
  case object BP extends RegLoc
  case object R8 extends RegLoc
  case object R9 extends RegLoc
  case object R10 extends RegLoc
  case object R11 extends RegLoc
  case object R12 extends RegLoc
  case object R13 extends RegLoc
  case object R14 extends RegLoc
  case object R15 extends RegLoc

  val callerSaved: List[RegLoc] = List(C, D, R8, R9, A, R10, R11)
  val calleeSaved: List[RegLoc] = List(DI, SI, R12, R13, R14, R15)

}

sealed trait RegSize extends Product with Serializable

object RegSize {
  case object Byte extends RegSize
  case object Word extends RegSize
  case object DWord extends RegSize
  case object QWord extends RegSize
}
