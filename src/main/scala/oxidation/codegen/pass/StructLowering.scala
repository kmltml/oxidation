package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._
import oxidation.ir._

object StructLowering extends Pass {

  def name = "struct-lowering"

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

  def binding(reg: Register): F[Vector[Register]] = for {
    bindings <- F.inspect(_.bindings)
    r <- bindings.get(reg).map(F.pure).getOrElse {
      for {
        regs <- genRegs(reg.typ.asInstanceOf[Type.Struct])
        _ <- saveBinding(reg, regs)
      } yield regs
    }
  } yield r

  def decompose(v: Val): F[Vector[Val]] = v match {
    case Val.Struct(members) => F.pure(members)
    case Val.R(r) => Nested(binding(r)).map(Val.R(_) : Val).value
  }

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
            binding(reg).map(_.toList)
          case reg => F.pure(List(reg))
        }
        retDeconstruction <- dest match {
          case Some(destReg @ Register(_, _, Type.Struct(members))) =>
            binding(destReg).map(_.zipWithIndex.map {
              case (r, i) => Inst.Move(r, Op.Member(Val.R(destReg), i))
            })
          case _ => F.pure(Vector.empty)
        }
      } yield Inst.Eval(dest, Op.Call(fn, newParams.flatten)) +: retDeconstruction

    case Inst.Move(dest, Op.Copy(Val(src, struct: Type.Struct))) =>
      for {
        destregs <- binding(dest)
        srcs <- decompose(src)
      } yield (destregs zip srcs).map {
        case (r, v) => Inst.Move(r, Op.Copy(v))
      }

    case Inst.Move(dest, Op.Member(Val.R(src), i)) =>
      for {
        bindings <- binding(src)
      } yield Vector(Inst.Move(dest, Op.Copy(Val.R(bindings(i)))))

    case Inst.Move(dest, Op.StructCopy(Val.R(src), substs)) =>
      for {
        srcRegs <- binding(src)
        destRegs <- binding(dest)
      } yield destRegs.zipWithIndex.map {
        case (d, i) =>
          val srcVal = (substs orElse srcRegs.andThen(Val.R))(i)
          Inst.Move(d, Op.Copy(srcVal))
      }

    case Inst.Move(dest @ Register(_, _, struct: Type.Struct), Op.Load(addr, offset)) =>
      for {
        ptr <- genReg(Type.Ptr)
        regs <- binding(dest)
        loads = regs.zipWithIndex.map {
          case (r, i) => Inst.Move(r, Op.Load(Val.R(ptr), Val.I(struct.offset(i), Type.I64)))
        }
      } yield Inst.Move(ptr, Op.Binary(InfixOp.Add, addr, offset)) +: loads

    case Inst.Eval(_, Op.Store(addr, offset, Val(value, struct: Type.Struct))) =>
      for {
        ptr <- genReg(Type.Ptr)
        srcs <- decompose(value)
        stores = srcs.zipWithIndex.map {
          case (src, i) => Inst.Do(Op.Store(Val.R(ptr), Val.I(struct.offset(i), Type.I64), src))
        }
      } yield Inst.Move(ptr, Op.Binary(InfixOp.Add, addr, offset)) +: stores

    case inst @ Inst.Move(Register(_, _, Type.Struct(_)), _) => throw new NotImplementedError(s"can't lower struct result in instruction $inst")
  }

  override def onFlow: FlowControl =?> F[FlowControl] = {
    case FlowControl.Return(Val.R(src @ Register(_, _, Type.Struct(_)))) =>
      F.inspect(s => FlowControl.Return(Val.Struct(s.bindings(src).map(Val.R))))
  }

}
