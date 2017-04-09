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

  def name = "amd64-backend"

  object BackendReg extends RegisterNamespace {
    val prefix = "br"
  }

  final case class St(nextReg: Int = 0, returnedStructs: Set[Register] = Set.empty)

  type F[A] = WriterT[State[St, ?], Set[(Register, RegLoc)], A]
  val F = MonadWriter[F, Set[(Register, RegLoc)]]
  val S = new MonadState[F, St] {
    override def get: F[St] = WriterT.lift(State.get)

    override def set(s: St): F[Unit] = WriterT.lift(State.set(s))

    override def pure[A](x: A): F[A] = F.pure(x)

    override def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: A => F[Either[A, B]]): F[B] = F.tailRecM(a)(f)
  }
  def extract[A](f: F[A]) = f.run.runA(St()).value._2

  private def nextReg(typ: Type): F[Register] = WriterT.lift(State {
    s => (s.copy(nextReg = s.nextReg + 1), Register(BackendReg, s.nextReg, typ))
  })

  private def saveReturnedStruct(reg: Register): F[Unit] =
    S.modify(s => s.copy(returnedStructs = s.returnedStructs + reg))

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
            case InfixOp.Div => RegLoc.A
            case InfixOp.Mod => RegLoc.D
          }),
          otherTemp -> (op match {
            case InfixOp.Mod => RegLoc.A
            case InfixOp.Div => RegLoc.D
          }),
          ltemp -> RegLoc.A,
          sextTemp -> RegLoc.D
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
          destTemp -> RegLoc.A,
          otherTemp -> RegLoc.D,
          ltemp -> RegLoc.A,
          sextTemp -> RegLoc.D
        ))
      } yield Vector(
        Inst.Move(ltemp, Op.Copy(l)),
        Inst.Move(sextTemp, Op.Copy(ir.Val.I(0, l.typ))),
        Inst.Move(rtemp, Op.Copy(r)),
        Inst.Move(destTemp, Op.Arith(InfixOp.Mul, ir.Val.R(ltemp), ir.Val.R(rtemp))),
        Inst.Move(otherTemp, Op.Garbled),
        Inst.Move(dest, Op.Copy(ir.Val.R(destTemp)))
      )

    case Inst.Eval(dest, call @ Op.Call(_, params)) =>
      val paramColours = params.zipWithIndex.collect {
        case (r, 0) => r -> RegLoc.C
        case (r, 1) => r -> RegLoc.D
        case (r, 2) => r -> RegLoc.R8
        case (r, 3) => r -> RegLoc.R9
      }
      for {
        _ <- F.tell(paramColours.toSet)
        destInsts <- dest match {
          case Some(r @ Register(_, _, Type.Struct(_))) => saveReturnedStruct(r).as(Vector.empty)
          case Some(r) => F.tell(Set(r -> RegLoc.A)).as(Vector())
          case None => for {
            r <- nextReg(Type.U0)
            _ <- F.tell(Set(r -> RegLoc.A))
          } yield Vector(Inst.Move(r, Op.Garbled))
        }
        newDest = dest match {
          case Some(Register(_, _, Type.Struct(_))) => None
          case x => x
        }
      } yield Inst.Eval(newDest, call) +: destInsts

    case Inst.Move(dest, Op.Member(ir.Val.R(src), member)) =>
      for {
        returnedStructs <- S.inspect(_.returnedStructs)
        _ = assert(returnedStructs contains src)
        _ <- F.tell(Set(dest -> (member match {
          case 0 => RegLoc.A
          case 1 => RegLoc.D
          case 2 => RegLoc.C
          case 3 => RegLoc.R8
          case 4 => RegLoc.R9
        })))
      } yield Vector(Inst.Move(dest, Op.Garbled))

    case inst @ Inst.Move(dest, Op.Arith(InfixOp.Add | InfixOp.Sub, l, r)) =>
      F.pure(Vector(
        inst, Inst.Do(Op.Copy(r))
      ))
  }

  override def onFlow: FlowControl =?> F[FlowControl] = {
    case flow @ FlowControl.Return(ir.Val.Struct(members)) =>
      val allocs = members.map { case ir.Val.R(r) => r }.zipWithIndex.map {
        case (r, 0) => r -> RegLoc.A
        case (r, 1) => r -> RegLoc.D
        case (r, 2) => r -> RegLoc.C
        case (r, 3) => r -> RegLoc.R8
        case (r, 4) => r -> RegLoc.R9
      }.toSet
      F.tell(allocs).as(flow)
  }

  override def onBlock: Block =?> F[Vector[Block]] = {
    case b @ Block(_, _, FlowControl.Return(ir.Val.R(r))) =>
      F.tell(Set(r -> RegLoc.A)).as(Vector(b))
  }

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, _, _, _) =>
      F.tell(params.zipWithIndex.collect {
        case (r, 0) => r -> RegLoc.C
        case (r, 1) => r -> RegLoc.D
        case (r, 2) => r -> RegLoc.R8
        case (r, 3) => r -> RegLoc.R9
      }.toSet).as(Vector(fun))
  }
}
