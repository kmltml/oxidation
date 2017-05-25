package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir.Type
import Reg._

case class StackLayout(shadowSpace: Vector[StackAlloc], mainSpace: Vector[StackAlloc]) {

  val allocatedShadowSlots: Int = shadowSpace.map(_.slots).sum
  require(allocatedShadowSlots <= 4)
  def freeShadowSlots: Int = 4 - allocatedShadowSlots

  lazy val shadowOffset: Vector[Int] = shadowSpace.scanLeft(0)(_ + _.slots)
  lazy val mainOffset: Vector[Int] = mainSpace.scanLeft(0)(_ + _.slots)

  def offset(addr: StackAlloc.Addr): Val.M = addr match {
    case StackAlloc.Main(i) => Val.m(None, RBP, -8 - i * 8)
    case StackAlloc.Shadow(i) => Val.m(None, RBP, 0x10 + i * 8)
  }

  def mainSize: Int = mainOffset.last

  def allocs: Vector[(StackAlloc, StackAlloc.Addr)] =
    shadowSpace.zipWithIndex.map(_.map(StackAlloc.Shadow)) ++
    mainSpace.zipWithIndex.map(_.map(StackAlloc.Main))

  def alloc(a: StackAlloc): (StackLayout, StackAlloc.Addr) =
    if(a.slots <= freeShadowSlots)
      (copy(shadowSpace = shadowSpace :+ a), StackAlloc.Shadow(shadowSpace.size))
    else
      (copy(mainSpace = mainSpace :+ a), StackAlloc.Main(mainSpace.size))


}

object StackLayout {

  def empty: StackLayout = StackLayout(Vector.empty, Vector.empty)

}

sealed abstract class StackAlloc(val slots: Int)

object StackAlloc {

  final case class SavedIntReg(reg: RegLoc) extends StackAlloc(1)
  final case class SavedXmmReg(reg: Xmm) extends StackAlloc(2)
  final case class Array(typ: Type.Arr) extends StackAlloc((typ.member.size * typ.elems + 7) / 8) // Round up to next 8 bytes
  final case object Spill extends StackAlloc(1)

  sealed trait Addr
  final case class Main(index: Int) extends Addr
  final case class Shadow(index: Int) extends Addr

}
