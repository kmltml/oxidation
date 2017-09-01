package oxidation
package codegen.pass
package ssa

import cats._
import cats.data._
import cats.implicits._

import codegen.Name
import ir._

object FromSSA extends Pass {

  override val name = "from-ssa"

  type Ctxt = Multimap[Name, (Register, Register)]

  override type F[A] = State[Ctxt, A]
  override val F = implicitly[MonadState[F, Ctxt]]
  override def extract[A](fa: F[A]): A = fa.runA(Map.empty).value

  override val onDef = {
    case fun @ Def.Fun(_, _, _, blocks, _) =>
      val writes: Ctxt = blocks.foldMap(_.instructions.collect {
        case Inst.Move(dest, Op.Phi(srcs)) =>
          srcs.mapValues(src => Set((dest, src)))
      }.combineAll)
      F.set(writes) as Vector(fun)
  }

  override val onBlock = {
    case block @ Block(name, instructions, _) =>
      F.inspect { ctxt =>
        val writes = ctxt.getOrElse(name, Set.empty)
        val writeInsts = writes.toVector.map {
          case (dest, src) => Inst.Move(dest, Op.Copy(Val.R(src)))
        }
        Vector(block.copy(instructions = instructions ++ writeInsts))
      }
  }

  override val onInstruction = {
    case Inst.Move(_, Op.Phi(_)) => F.pure(Vector.empty)
  }

}
