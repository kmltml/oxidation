package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import codegen.pass.Pass
import oxidation.ir._
import Reg._

object Amd64BackendPass extends Pass {

  object BackendReg extends RegisterNamespace {
    val prefix = "br"
  }

  type F[A] = WriterT[State[Int, ?], Set[(Register, Reg)], A]
  val F = MonadWriter[F, Set[(Register, Reg)]]
  val S = new MonadState[F, Int] {
    override def get: F[Int] = WriterT.lift(State.get)

    override def set(s: Int): F[Unit] = WriterT.lift(State.set(s))

    override def pure[A](x: A): F[A] = F.pure(x)

    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
  }
  def extract[A](f: F[A]) = f.run.runA(0).value._2

  private def nextReg(typ: Type): F[Register] = WriterT.lift(State { s => (s + 1, Register(BackendReg, s, typ))} )

  override def onInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Move(dest, Op.Arith(op @ (InfixOp.Div | InfixOp.Mod) , l, r)) => // TODO handle signed case; deduplicate?
      for {
        ltemp <- nextReg(l.typ)
        sextTemp <- nextReg(l.typ)
        rtemp <- nextReg(r.typ)
        destTemp <- nextReg(dest.typ)
        otherTemp <- nextReg(dest.typ)
        _ <- F.tell(Set(
          destTemp -> (op match {
            case InfixOp.Div => RAX
            case InfixOp.Mod => RDX
          }),
          otherTemp -> (op match {
            case InfixOp.Mod => RAX
            case InfixOp.Div => RDX
          }),
          ltemp -> RAX,
          sextTemp -> RDX
        ))
      } yield Vector(
        Inst.Move(ltemp, Op.Copy(l)),
        Inst.Move(sextTemp, Op.Copy(ir.Val.I(0, l.typ))),
        Inst.Move(rtemp, Op.Copy(r)),
        Inst.Do(Op.Copy(ir.Val.R(sextTemp))),
        Inst.Move(destTemp, Op.Arith(op, ir.Val.R(ltemp), ir.Val.R(rtemp))),
        Inst.Move(otherTemp, Op.Garbled),
        Inst.Move(dest, Op.Copy(ir.Val.R(destTemp)))
      )
    case Inst.Move(dest, Op.Arith(InfixOp.Mul , l, r)) => // TODO handle signed case
      for {
        ltemp <- nextReg(l.typ)
        sextTemp <- nextReg(l.typ)
        rtemp <- nextReg(r.typ)
        destTemp <- nextReg(dest.typ)
        otherTemp <- nextReg(dest.typ)
        _ <- F.tell(Set(
          destTemp -> RAX,
          otherTemp -> RDX,
          ltemp -> RAX,
          sextTemp -> RDX
        ))
      } yield Vector(
        Inst.Move(ltemp, Op.Copy(l)),
        Inst.Move(sextTemp, Op.Copy(ir.Val.I(0, l.typ))),
        Inst.Move(rtemp, Op.Copy(r)),
        Inst.Move(destTemp, Op.Arith(InfixOp.Mul, ir.Val.R(ltemp), ir.Val.R(rtemp))),
        Inst.Move(otherTemp, Op.Garbled),
        Inst.Move(dest, Op.Copy(ir.Val.R(destTemp)))
      )
  }

  override def onBlock: Block =?> F[Vector[Block]] = {
    case b @ Block(_, _, FlowControl.Return(ir.Val.R(r))) =>
      F.tell(Set(r -> RAX)).as(Vector(b))
  }

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, _, _) =>
      F.tell(params.zipWithIndex.collect {
        case (r, 0) => r -> RCX
        case (r, 1) => r -> RDX
        case (r, 2) => r -> R8
        case (r, 3) => r -> R9
      }.toSet).as(Vector(fun))
  }
}
