package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir._

import oxidation.codegen.pass.Pass

object RegisterSpillPass extends Pass {

  override def name = "register-spill"

  case object SpillFillReg extends RegisterNamespace {
    override def prefix = "sf"
  }

  override type F[A] = StateT[Reader[Set[Register], ?], Int, A]
  override val F: MonadState[F, Int] = implicitly[MonadState[F, Int]]

  override def extract[A](f: F[A]): A = ???

  val spilledRegisters: F[Set[Register]] = StateT.lift(Reader(identity))

  def nextReg(t: Type): F[Register] = for {
    n <- F.get
    _ <- F.modify(_ + 1)
  } yield Register(SpillFillReg, n, t)

  def fill(v: ir.Val): F[(Vector[Inst], ir.Val)] = for {
    spilled <- spilledRegisters
    res <- v match {
      case ir.Val.R(r) if spilled(r) =>
        nextReg(r.typ).map(t => (Vector(Inst.Move(t, Op.Copy(v))), ir.Val.R(t)))
      case _ => F.pure((Vector.empty, v))
    }
  } yield res

  def spill(r: Register): F[(Register, Vector[Inst])] = for {
    spilled <- spilledRegisters
    res <-
      if (spilled(r))
        nextReg(r.typ).map(t => (t, Vector(Inst.Move(r, Op.Copy(ir.Val.R(t))))))
      else F.pure((r, Vector.empty))
  } yield res

  override def onInstruction = {
    case Inst.Do(Op.Store(addr, offset, value)) =>
      for {
        a <- fill(addr)
        o <- fill(offset)
        v <- fill(value)
      } yield a._1 ++ o._1 ++ v._1 :+ Inst.Do(Op.Store(a._2, o._2, v._2))

    case Inst.Move(dest, Op.Load(addr, offset)) =>
      for {
        a <- fill(addr)
        o <- fill(offset)
        d <- spill(dest)
      } yield (a._1 ++ o._1 :+ Inst.Move(d._1, Op.Load(a._2, o._2))) ++ d._2
  }

}
