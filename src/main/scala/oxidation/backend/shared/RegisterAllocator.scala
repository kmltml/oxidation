package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._

import codegen.ir

class RegisterAllocator[Reg](val calleeSavedRegs: List[Reg], val callerSavedRegs: List[Reg]) {

  sealed trait Alloc
  final case class R(r: Reg) extends Alloc
  final case class Stack(offset: Int) extends Alloc

  def allocate(blocks: Vector[ir.Block], preallocated: Map[ir.Register, Alloc]): Map[ir.Register, Alloc] = {
    // Placeholder no-op register allocation here
    val irRegs = for {
      block <- blocks
      inst <- block.instructions
      r <- inst match {
        case ir.Inst.Eval(Some(r), _) => Vector(r)
        case _ => Vector.empty
      }
    } yield r

    type S[A] = State[(List[Reg], Int), A]
    val S = MonadState[S, (List[Reg], Int)]

    val freeRegs: List[Reg] = (callerSavedRegs ++ calleeSavedRegs) diff preallocated.values.collect {
      case R(r) => r
    }.toList

    irRegs.traverse(r => for {
      free <- S.get
      reg <- free match {
        case (Nil, i) => S.set((Nil, i + 1)).as(Stack(i): Alloc)
        case (h :: t, i) => S.set((t, i)).as(R(h): Alloc)
      }
    } yield r -> reg).runA((freeRegs, 0)).value.toMap ++ preallocated
  }

}
