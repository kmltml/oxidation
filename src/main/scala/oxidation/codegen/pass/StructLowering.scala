package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._
import oxidation.ir._

object StructLowering extends Pass {

  def name = "strruct-lowering"

  object StructLoweringReg extends RegisterNamespace {
    def prefix: String = "sl"
  }

  def register(index: Int, typ: Type): Register = Register(StructLoweringReg, index, typ)

  final case class S(nextReg: Int = 0, bindings: Map[Register, Vector[Register]] = Map.empty)

  override type F[A] = State[S, A]
  val F: MonadState[F, S] = implicitly[MonadState[F, S]]

  override def extract[A](f: F[A]): A = f.runA(S()).value


  def genReg(typ: Type): F[Register] = for {
    i <- F.inspect(_.nextReg)
    _ <- F.modify(_.copy(nextReg = i + 1))
  } yield register(i, typ)

  def genRegs(struct: Type.Struct): F[Vector[Register]] =
    struct.members.traverse(genReg)

  def saveBinding(before: Register, after: Vector[Register]): F[Unit] =
    F.modify(s => s.copy(bindings = s.bindings.updated(before, after)))

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, _, _, _) => for {
      _ <- F.set(S())
      newParams <- params.traverse {
        case r @ Register(_, _, t: Type.Struct) => for {
          regs <- genRegs(t)
          _ <- saveBinding(r, regs)
        } yield regs.toList
        case r => F.pure(List(r))
      }
    } yield Vector(fun.copy(params = newParams.flatten))
  }

  override def onInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Eval(dest, Op.Call(fn, params)) =>
      for {
        newParams <- params.traverse {
          case reg @ Register(_, _, Type.Struct(_)) =>
            F.inspect(_.bindings).map(_(reg).toList)
          case reg => F.pure(List(reg))
        }
      } yield Vector(Inst.Eval(dest, Op.Call(fn, newParams.flatten))) // TODO handle functions returning structs

    case Inst.Move(dest, Op.Copy(Val.Struct(memVals))) =>
      for {
        regs <- memVals.traverse { v => genReg(v.typ).map(r => v -> r) }
        _ <- saveBinding(dest, regs.map(_._2))
      } yield regs.map { case (v, r) => Inst.Move(r, Op.Copy(v)) }

    case Inst.Move(dest, Op.Member(Val.R(src), i)) =>
      for {
        bindings <- F.inspect(_.bindings)
      } yield Vector(Inst.Move(dest, Op.Copy(Val.R(bindings(src)(i)))))

    case Inst.Move(dest @ Register(_, _, Type.Struct(_)), Op.Copy(Val.R(src))) =>
      for {
        srcRegs <- F.inspect(_.bindings(src))
        regs <- srcRegs.traverse(r => genReg(r.typ).map(_ -> r))
        _ <- saveBinding(dest, regs.map(_._1))
      } yield regs.map { case (r, v) => Inst.Move(r, Op.Copy(Val.R(v))) }

    case inst @ Inst.Move(Register(_, _, Type.Struct(_)), _) => throw new NotImplementedError(s"can't lower struct result in instruction $inst")
  }

  override def onFlow: FlowControl =?> F[FlowControl] = {
    case FlowControl.Return(Val.R(src @ Register(_, _, Type.Struct(_)))) =>
      F.inspect(s => FlowControl.Return(Val.Struct(s.bindings(src).map(Val.R))))
  }

}
