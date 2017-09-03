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

  final case class St(nextReg: Int = 0, returnedStructs: Map[Register, Vector[Register]] = Map.empty)
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

  private def returnedStructColors(members: Vector[Type]): Vector[AnyReg] = {
    val intColours = List(RegLoc.A, RegLoc.D, RegLoc.C, RegLoc.R8, RegLoc.R9)
    val floatColours = List(Xmm0, Xmm1, Xmm2, Xmm3)
    members.foldLeft((Vector.empty[AnyReg], intColours, floatColours)) {
      case ((acc, ics, c :: fcs), _: Type.F) => (acc :+ c, ics, fcs)
      case ((acc, c :: ics, fcs), _) => (acc :+ c, ics, fcs)
    }._1
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
      for {
        regParams <- params.take(4).toVector.traverse { v =>
          nextReg(v.typ).map(d => (Inst.Move(d, Op.Copy(v)), d))
        }
        paramColours = regParams.zipWithIndex.collect {
          case ((_, r @ Register(_, _, _: Type.Integral | Type.U1 | Type.Ptr)), i) => (r, i)
        }.map {
          case (r, 0) => r -> RegLoc.C
          case (r, 1) => r -> RegLoc.D
          case (r, 2) => r -> RegLoc.R8
          case (r, 3) => r -> RegLoc.R9
        }
        floatParamColours = regParams.zipWithIndex.collect {
          case ((_, r @ Register(_, _, _: Type.F)), i) => (r, i)
        }.map {
          case (r, 0) => r -> Xmm0
          case (r, 1) => r -> Xmm1
          case (r, 2) => r -> Xmm2
          case (r, 3) => r -> Xmm3
        }
        _ <- F.tell(Colours(paramColours.toSet, floatParamColours.toSet))
        newDest <- dest match {
          case Some(r @ Register(_, _, Type.Struct(members))) =>
            for {
              regs <- members.traverse(nextReg)
              colors = returnedStructColors(members)
              _ <- (regs zip colors).traverse {
                case (r, x: Xmm) => tellFloatColour(Set(r -> x))
                case (r, l: RegLoc) => tellIntColour(Set(r -> l))
              }
              copies = regs.map(r => Inst.Move(r, Op.Garbled))
              _ <- S.modify(s => s.copy(returnedStructs = s.returnedStructs + (r -> regs)))
            } yield (copies, None)
          case Some(r) =>
            for {
              d <- nextReg(r.typ)
              _ <- r.typ match {
                case _: Type.Integral | Type.U1 | Type.Ptr =>
                  tellIntColour(Set(d -> RegLoc.A))
                case _: Type.F =>
                  tellFloatColour(Set(d -> Xmm0))
              }
            } yield (Vector(Inst.Move(r, Op.Copy(ir.Val.R(d)))), Some(d))
            
          case None => F.pure((Vector.empty, None))
        }
        stackParams <- params.drop(4).traverse {
          case d: ir.Val.F64 => // You can't just have a 64-bit immediate, that would be too easy!
            nextReg(Type.F64).map(r => (Vector(Inst.Move(r, Op.Copy(d)): Inst), ir.Val.R(r): ir.Val))
          case v => F.pure((Vector.empty[Inst], v))
        }
        newCall = call.copy(params = regParams.map(t => ir.Val.R(t._2)).toList ++ stackParams.map(_._2))
      } yield regParams.map(_._1) ++ stackParams.flatMap(_._1) ++ Vector(Inst.Eval(newDest._2, newCall)) ++ newDest._1

    case Inst.Move(dest, Op.Member(ir.Val.R(src), member)) =>
      for {
        r <- S.inspect(_.returnedStructs(src)(member))
      } yield Vector(Inst.Move(dest, Op.Copy(ir.Val.R(r))))

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
      for {
        regs <- members.traverse(v => nextReg(v.typ).map(d => (Inst.Move(d, Op.Copy(v)), d)))
        (ints, floats) = regs.map(_._2).partition {
          case r @ ir.Register(_, _, _: ir.Type.F) => false
          case r => true
        }
        intColours = List(RegLoc.A, RegLoc.D, RegLoc.C, RegLoc.R8, RegLoc.R9)
        _ = assert(ints.size <= intColours.size)
        intAllocs = (ints zip intColours).toSet
        floatColours = List(Xmm0, Xmm1, Xmm2, Xmm3)
        _ = assert(floats.size <= floatColours.size)
        floatAllocs = (floats zip floatColours).toSet
        _ <- F.tell(Colours(intAllocs, floatAllocs))
      } yield (regs.map(_._1), FlowControl.Return(ir.Val.Struct(regs.map(t => ir.Val.R(t._2)))))
    case flow @ FlowControl.Return(ir.Val.I(_, Type.U0)) =>
      F.pure((Vector.empty, flow))
    case flow @ FlowControl.Return(v) =>
      for {
        d <- nextReg(v.typ)
        _ <- v.typ match {
          case _: Type.F => tellFloatColour(Set(d -> Xmm0))
          case _ => tellIntColour(Set(d -> RegLoc.A))
        }
      } yield (Vector(Inst.Move(d, Op.Copy(v))), FlowControl.Return(ir.Val.R(d)))
  }

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, _, blocks, _) =>
      for {
        newParams <- params.traverse(p => nextReg(p.typ))
        paramColours = newParams.zipWithIndex.collect {
          case t @ (Register(_, _, _: Type.Integral | Type.U1 | Type.Ptr), _) => t
        }.collect {
          case (r, 0) => r -> RegLoc.C
          case (r, 1) => r -> RegLoc.D
          case (r, 2) => r -> RegLoc.R8
          case (r, 3) => r -> RegLoc.R9
        }
        floatParamColours = newParams.zipWithIndex.collect {
          case t @ (Register(_, _, _: Type.F), _) => t
        }.collect {
          case (r, 0) => r -> Xmm0
          case (r, 1) => r -> Xmm1
          case (r, 2) => r -> Xmm2
          case (r, 3) => r -> Xmm3
        }
        _ <- F.tell(Colours(paramColours.toSet, floatParamColours.toSet))
        paramCopies = (params zip newParams).map {
          case (d, s) => Inst.Move(d, Op.Copy(ir.Val.R(s)))
        }.toVector
        firstBlock = blocks.head
        newFirstBlock = firstBlock.copy(instructions = paramCopies ++ firstBlock.instructions)
      } yield Vector(fun.copy(body = newFirstBlock +: blocks.tail, params = newParams))
  }

  override def onVal = {
    case v @ ir.Val.I(l, t) if l > Int.MaxValue && l < Int.MinValue =>
      for {
        r <- nextReg(t)
        s <- nextReg(t)
        _ <- tellIntColour(Set(r -> RegLoc.A))
      } yield (Vector(
        Inst.Move(r, Op.Copy(v)),
        Inst.Move(s, Op.Copy(ir.Val.R(r))) // make r's lifetime as short as possible
      ), ir.Val.R(s))
  }

}
