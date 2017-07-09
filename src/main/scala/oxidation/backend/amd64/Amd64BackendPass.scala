package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._
import codegen.pass.Pass
import oxidation.ir._
import Reg._
import Xmm._

object Amd64BackendPass extends Pass {

  def name = "amd64-backend"

  object BackendReg extends RegisterNamespace {
    val prefix = "br"
  }

  final case class St(nextReg: Int = 0, returnedStructs: Map[Register, Vector[AnyReg]] = Map.empty)
  final case class Colours(int: Set[(Register, RegLoc)] = Set.empty,
                           float: Set[(Register, Xmm)] = Set.empty)
  implicit object Colours extends Monoid[Colours] {

    override def empty: Colours = Colours(Set.empty, Set.empty)

    override def combine(x: Colours, y: Colours): Colours = Colours(x.int |+| y.int, x.float |+| y.float)

  }

  type F[A] = WriterT[State[St, ?], Colours, A]

  val F = MonadWriter[F, Colours]
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

  private def saveReturnedStruct(reg: Register): F[Unit] = {
    val Type.Struct(members) = reg.typ
    val intColours = List(RegLoc.A, RegLoc.D, RegLoc.C, RegLoc.R8, RegLoc.R9)
    val floatColours = List(Xmm0, Xmm1, Xmm2, Xmm3)
    val memberColours = members.foldLeft((Vector.empty[AnyReg], intColours, floatColours)) {
      case ((acc, ics, c :: fcs), _: Type.F) => (acc :+ c, ics, fcs)
      case ((acc, c :: ics, fcs), _) => (acc :+ c, ics, fcs)
    }._1
    S.modify(s => s.copy(returnedStructs = s.returnedStructs + (reg -> memberColours)))
  }

  private def tellIntColour(cs: Set[(Register, RegLoc)]): F[Unit] = F.tell(Colours(int = cs))
  private def tellFloatColour(cs: Set[(Register, Xmm)]): F[Unit] = F.tell(Colours(float = cs))

  override def onInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Move(dest @ ir.Register(_, _, _: ir.Type.Integral), Op.Binary(op @ (InfixOp.Div | InfixOp.Mod) , l, r)) => // TODO handle signed case; deduplicate?
      for {
        ltemp <- nextReg(l.typ)
        sextTemp <- nextReg(l.typ)
        rtemp <- nextReg(r.typ)
        destTemp <- nextReg(dest.typ)
        otherTemp <- nextReg(dest.typ)
        _ <- tellIntColour(Set(
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
        Inst.Move(rtemp, Op.Copy(r)),
        Inst.Move(ltemp, Op.Copy(l)),
        Inst.Move(sextTemp, Op.Copy(ir.Val.I(0, l.typ))),
        Inst.Do(Op.Copy(ir.Val.R(sextTemp))),
        Inst.Move(destTemp, Op.Binary(op, ir.Val.R(ltemp), ir.Val.R(rtemp))),
        Inst.Move(otherTemp, Op.Garbled),
        Inst.Move(dest, Op.Copy(ir.Val.R(destTemp)))
      )
    case Inst.Move(dest @ ir.Register(_, _, _: ir.Type.Integral), Op.Binary(InfixOp.Mul , l, r)) => // TODO handle signed case
      for {
        ltemp <- nextReg(l.typ)
        sextTemp <- nextReg(l.typ)
        rtemp <- nextReg(r.typ)
        destTemp <- nextReg(dest.typ)
        otherTemp <- nextReg(dest.typ)
        _ <- tellIntColour(Set(
          destTemp -> RegLoc.A,
          otherTemp -> RegLoc.D,
          ltemp -> RegLoc.A,
          sextTemp -> RegLoc.D
        ))
      } yield Vector(
        Inst.Move(rtemp, Op.Copy(r)),
        Inst.Move(ltemp, Op.Copy(l)),
        Inst.Move(sextTemp, Op.Copy(ir.Val.I(0, l.typ))),
        Inst.Move(destTemp, Op.Binary(InfixOp.Mul, ir.Val.R(ltemp), ir.Val.R(rtemp))),
        Inst.Move(otherTemp, Op.Garbled),
        Inst.Move(dest, Op.Copy(ir.Val.R(destTemp)))
      )

    case Inst.Move(dest, Op.Binary(op @ (InfixOp.Shr | InfixOp.Shl), left, right: ir.Val.R)) =>
      for {
        r <- nextReg(right.typ)
        _ <- tellIntColour(Set(r -> RegLoc.C))
      } yield Vector(
        Inst.Move(r, Op.Copy(right)),
        Inst.Move(dest, Op.Binary(op, left, ir.Val.R(r))),
        Inst.Do(Op.Copy(ir.Val.R(r)))
      )

    case Inst.Eval(dest, call @ Op.Call(_, params)) =>
      val paramColours = params.zipWithIndex.collect {
        case t @ (Register(_, _, _: Type.Integral | Type.U1 | Type.Ptr), _) => t
      }.collect {
        case (r, 0) => r -> RegLoc.C
        case (r, 1) => r -> RegLoc.D
        case (r, 2) => r -> RegLoc.R8
        case (r, 3) => r -> RegLoc.R9
      }
      val floatParamColours = params.zipWithIndex.collect {
        case t @ (Register(_, _, _: Type.F), _) => t
      }.collect {
        case (r, 0) => r -> Xmm0
        case (r, 1) => r -> Xmm1
        case (r, 2) => r -> Xmm2
        case (r, 3) => r -> Xmm3
      }
      for {
        _ <- F.tell(Colours(paramColours.toSet, floatParamColours.toSet))
        destInsts <- dest match {
          case Some(r @ Register(_, _, Type.Struct(_))) => saveReturnedStruct(r).as(Vector.empty)
          case Some(r @ Register(_, _, _: Type.F)) => tellFloatColour(Set(r -> Xmm0)).as(Vector())
          case Some(r) => tellIntColour(Set(r -> RegLoc.A)).as(Vector())
          case None => F.pure(Vector.empty)
        }
        newDest = dest match {
          case Some(Register(_, _, Type.Struct(_))) => None
          case x => x
        }
      } yield Inst.Eval(newDest, call) +: destInsts

    case Inst.Move(dest, Op.Member(ir.Val.R(src), member)) =>
      for {
        members <- S.inspect(_.returnedStructs(src))
        _ <- members(member) match {
          case x: Xmm => tellFloatColour(Set(dest -> x))
          case r: RegLoc => tellIntColour(Set(dest -> r))
        }
      } yield Vector(Inst.Move(dest, Op.Garbled))

    case inst @ Inst.Move(dest, Op.Binary(InfixOp.Add | InfixOp.Sub | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Xor | InfixOp.Shl | InfixOp.Shr, l, r)) =>
      F.pure(Vector(
        inst, Inst.Do(Op.Copy(r)) // Prevent `dest` and `r` from being allocated to the same register, it would otherwise end up as `mov x, l / add x, x`
      ))


    case Inst.Move(dest, Op.Binary(op @ (InfixOp.Eq | InfixOp.Neq), ir.Val(l, _: ir.Type.F), r)) =>
      for {
        mask <- nextReg(l.typ)
      } yield Vector(
        Inst.Move(mask, Op.Copy(l)),
        Inst.Move(dest, Op.Binary(op, ir.Val.R(mask), r))
      )

    case Inst.Eval(dest, Op.Store(addr, off, f @ (ir.Val.F32(_) | ir.Val.F64(_)))) =>
      for {
        t <- nextReg(f.typ)
      } yield Vector(
        Inst.Move(t, Op.Copy(f)),
        Inst.Eval(dest, Op.Store(addr, off, ir.Val.R(t)))
      )
  }

  override def onFlow = {
    case flow @ FlowControl.Return(ir.Val.Struct(members)) =>
      val regs = members.map { case ir.Val.R(r) => r }
      val (ints, floats) = regs.partition {
        case r @ ir.Register(_, _, _: ir.Type.F) => false
        case r => true
      }
      val intColours = List(RegLoc.A, RegLoc.D, RegLoc.C, RegLoc.R8, RegLoc.R9)
      assert(ints.size <= intColours.size)
      val intAllocs = (ints zip intColours).toSet
      val floatColours = List(Xmm0, Xmm1, Xmm2, Xmm3)
      assert(floats.size <= floatColours.size)
      val floatAllocs = (floats zip floatColours).toSet
      F.tell(Colours(intAllocs, floatAllocs)) as (Vector.empty, flow)
    case flow @ FlowControl.Return(ir.Val.R(r)) =>
      val c = r.typ match {
        case _: Type.F => tellFloatColour(Set(r -> Xmm0))
        case _ => tellIntColour(Set(r -> RegLoc.A))
      }
      c as (Vector.empty, flow)
  }

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, _, _, _) =>
      val paramColours = params.zipWithIndex.collect {
        case t @ (Register(_, _, _: Type.Integral | Type.U1 | Type.Ptr), _) => t
      }.collect {
        case (r, 0) => r -> RegLoc.C
        case (r, 1) => r -> RegLoc.D
        case (r, 2) => r -> RegLoc.R8
        case (r, 3) => r -> RegLoc.R9
      }
      val floatParamColours = params.zipWithIndex.collect {
        case t @ (Register(_, _, _: Type.F), _) => t
      }.collect {
        case (r, 0) => r -> Xmm0
        case (r, 1) => r -> Xmm1
        case (r, 2) => r -> Xmm2
        case (r, 3) => r -> Xmm3
      }
      F.tell(Colours(paramColours.toSet, floatParamColours.toSet)).as(Vector(fun))
  }
}
