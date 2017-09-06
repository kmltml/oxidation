package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir._

import oxidation.codegen.pass.Pass

class RegisterSpillPass extends Pass {

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

    case m @ Inst.Move(dest, Op.Copy(src)) =>
      for {
        spilled <- spilledRegisters
        res <- if(spilled(dest)) for {
          s <- fill(src)
        } yield s._1 :+ Inst.Move(dest, Op.Copy(s._2))
        else F.pure(Vector(m))
      } yield res

    case m @ Inst.Move(dest, o @ Op.Widen(ir.Val.R(src))) =>
      for {
        spilled <- spilledRegisters
        res <- if(spilled(src)) for {
          d <- spill(dest)
        } yield Inst.Move(d._1, o) +: d._2
        else F.pure(Vector(m))
      } yield res

    case Inst.Move(dest, Op.Binary(op @ (InfixOp.Eq | InfixOp.Neq | InfixOp.Gt | InfixOp.Geq | InfixOp.Lt | InfixOp.Leq),
                                   ir.Val.R(left), ir.Val.R(right))) =>
      for {
        spilled <- spilledRegisters
        l <- if(spilled(left) && spilled(right)) fill(ir.Val.R(left)) else F.pure((Vector.empty, ir.Val.R(left)))
      } yield l._1 :+ Inst.Move(dest, Op.Binary(op, l._2, ir.Val.R(right)))

    case Inst.Eval(dest, Op.Call(fn, params)) =>
      for {
        stackParams <- params.drop(4).traverse(r => fill(r))
        fillInsts = stackParams.flatMap(_._1).toVector
        newStackParams = stackParams.map {
          case (_, v) => v
        }
      } yield fillInsts :+ Inst.Eval(dest, Op.Call(fn, params.take(4) ++ newStackParams))

    case Inst.Move(dest, sa: Op.Stackalloc) =>
      for {
        s <- spill(dest)
      } yield Inst.Move(s._1, sa) +: s._2
  }

}
