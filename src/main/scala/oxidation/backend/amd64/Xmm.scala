package oxidation
package backend
package amd64

case class Xmm(i: Int) extends AnyReg

object Xmm {

  final val Xmm0  = Xmm(0)
  final val Xmm1  = Xmm(1)
  final val Xmm2  = Xmm(2)
  final val Xmm3  = Xmm(3)
  final val Xmm4  = Xmm(4)
  final val Xmm5  = Xmm(5)
  final val Xmm6  = Xmm(6)
  final val Xmm7  = Xmm(7)
  final val Xmm8  = Xmm(8)
  final val Xmm9  = Xmm(9)
  final val Xmm10 = Xmm(10)
  final val Xmm11 = Xmm(11)
  final val Xmm12 = Xmm(12)
  final val Xmm13 = Xmm(13)
  final val Xmm14 = Xmm(14)
  final val Xmm15 = Xmm(15)

  val callerSaved = List(Xmm0, Xmm1, Xmm2, Xmm3, Xmm4, Xmm5)
  val calleeSaved = List(Xmm6, Xmm7, Xmm8, Xmm9, Xmm10, Xmm11, Xmm12, Xmm13, Xmm14, Xmm15)

}
