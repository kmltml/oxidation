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

  final case class S(nextReg: Int = 0, bindings: Map[Register, Vector[Register]] = Map.empty, sret: Option[Register] = None)

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

  def flatten(v: Val): F[Vector[Val]] = v match {
    case Val.Struct(members) => members.traverse(flatten).map(_.flatten)
    case Val.R(r) => flatten(r).map(_.map(Val.R))
    case Val(v, _: Type.Struct) => throw new AssertionError(show"I don't know what to do with $v")
    case v => F.pure(Vector(v))
  }

  def flatten(r: Register): F[Vector[Register]] = r match {
    case Register(_, _, _: Type.Struct) => binding(r).flatMap(_.traverse(flatten).map(_.flatten))
    case r => F.pure(Vector(r))
  }


  def flatten(t: Type): Vector[Type] = t match {
    case Type.Struct(members) => members.flatMap(flatten)
    case t => Vector(t)
  }

  private def fitsInRegisters(s: Type): Boolean = {
    val (ints, floats) = flatten(s).foldMap {
      case _: Type.F => (0, 1)
      case _ => (1, 0)
    }
    ints <= 5 && floats <= 4
  }

  override def onDef: Def =?> F[Vector[Def]] = {
    case fun @ Def.Fun(_, params, ret, _, _) =>
      for {
        _ <- F.set(S())
        newParams <- params.traverse(flatten)
        newRet = flatten(ret) match {
          case Vector() => Type.U0
          case Vector(t) => t
          case ts => Type.Struct(ts)
        }
        res <-
          if(fitsInRegisters(newRet)) F.pure(fun.copy(params = newParams.flatten, ret = newRet))
          else for {
            r <- genReg(Type.Ptr)
            _ <- F.modify(_.copy(sret = Some(r)))
          } yield fun.copy(params = r +: newParams.flatten, ret = Type.Ptr)
      } yield Vector(res)
  }

  override def onInstruction: Inst =?> F[Vector[Inst]] = {
    case Inst.Eval(dest, Op.Call(fn, params)) =>
      for {
        newParams <- params.traverse(flatten).map(_.flatten)
        res <- dest match {
          case Some(destReg @ Register(_, _, retType: Type.Struct)) if fitsInRegisters(retType) =>
            def rebuild(reg: Register, t: Type.Struct, regs: List[Register]): F[(List[Register], Register)] = {
              t.members.foldM((regs, Vector.empty[Register])) {
                case ((regs, acc), t: Type.Struct) =>
                  for {
                    r <- genReg(t)
                    res <- rebuild(r, t, regs)
                  } yield res.map(acc :+ _)
                case ((r :: rest, acc), _) => F.pure(rest, acc :+ r)
              } flatMap {
                case (rest, members) => saveBinding(reg, members) as (rest, reg)
              }
            }

            for {
              flatr <- genReg(Type.Struct(flatten(destReg.typ)))
              allRegs <- flatten(destReg)
              _ <- saveBinding(flatr, allRegs)
              regMoves = allRegs.zipWithIndex.map {
                case (r, i) => Inst.Move(r, Op.Member(Val.R(flatr), i))
              }
              _ <- rebuild(destReg, retType, allRegs.toList)
            } yield Inst.Eval(Some(flatr), Op.Call(fn, newParams)) +: regMoves

          case Some(destReg @ Register(_, _, retType: Type.Struct)) =>
            for {
              sret <- genReg(Type.Ptr)
              newfun = fn match {
                case Val.G(name, Type.Fun(params, _)) => Val.G(name, Type.Fun(Type.Ptr +: params, Type.Ptr))
              }
              returned <- genReg(Type.Ptr)
              load <- txInstruction(Inst.Move(destReg, Op.Load(Val.R(returned), Val.I(0, Type.I64))))
            } yield Vector(
              Inst.Move(sret, Op.Stackalloc(retType.size)),
              Inst.Move(returned, Op.Call(newfun, Val.R(sret) +: newParams))
            ) ++ load

          case dest => F.pure(Vector(Inst.Eval(dest, Op.Call(fn, newParams))))
        }
      } yield res

    case Inst.Move(dest, Op.Copy(Val(src, struct: Type.Struct))) =>
      for {
        destregs <- binding(dest)
        srcs <- decompose(src)
        res <- (destregs zip srcs).traverse {
          case (r, v) => txInstruction(Inst.Move(r, Op.Copy(v)))
        }
      } yield res.flatten

    case Inst.Move(dest, Op.Member(Val.R(src), i)) =>
      for {
        bindings <- binding(src)
        res <- txInstruction(Inst.Move(dest, Op.Copy(Val.R(bindings(i)))))
      } yield res

    case Inst.Move(dest, Op.Member(Val.Struct(members), i)) =>
      F.pure(Vector(Inst.Move(dest, Op.Copy(members(i)))))

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
        loads <- regs.zipWithIndex.traverse {
          case (r, i) => txInstruction(Inst.Move(r, Op.Load(Val.R(ptr), Val.I(struct.offset(i), Type.I64))))
        }
      } yield Inst.Move(ptr, Op.Binary(InfixOp.Add, addr, offset)) +: loads.flatten

    case Inst.Eval(_, Op.Store(addr, offset, Val(value, struct: Type.Struct))) =>
      for {
        ptr <- genReg(Type.Ptr)
        srcs <- decompose(value)
        stores <- srcs.zipWithIndex.traverse {
          case (src, i) => txInstruction(Inst.Do(Op.Store(Val.R(ptr), Val.I(struct.offset(i), Type.I64), src)))
        }
      } yield Inst.Move(ptr, Op.Binary(InfixOp.Add, addr, offset)) +: stores.flatten

    case Inst.Eval(dest, Op.Binary(op @ (InfixOp.Eq | InfixOp.Neq), Val(left, struct: Type.Struct), Val(right, Type.Struct(_)))) =>
      for {
        lvals <- decompose(left)
        rvals <- decompose(right)
        eqs <- (struct.members, lvals, rvals).zipped.toList.toNel.get.traverse {
          case (t, lval, rval) => genReg(Type.U1).map(r => (r, Vector(Inst.Move(r, Op.Binary(op, lval, rval)))))
        }
        res <- eqs.reduceLeftM(F.pure) {
          case ((prevr, instsA), (thisr, instsB)) =>
            val combine = op match {
              case InfixOp.Eq => InfixOp.BitAnd
              case InfixOp.Neq => InfixOp.BitOr
            }
            genReg(Type.U1).map(r => (r, instsA ++ instsB :+ Inst.Move(r, Op.Binary(combine, Val.R(prevr), Val.R(thisr)))))
        }
        insts <- res._2.traverse(txInstruction)
      } yield insts.flatten :+ Inst.Eval(dest, Op.Copy(Val.R(res._1)))

    case inst @ Inst.Move(Register(_, _, Type.Struct(_)), _) => throw new NotImplementedError(s"can't lower struct result in instruction $inst")
  }

  override def onFlow: FlowControl =?> F[(Vector[ir.Inst], FlowControl)] = {
    case FlowControl.Return(Val.R(src @ Register(_, _, Type.Struct(_)))) =>
      for {
        sret <- F.inspect(_.sret)
        res <- sret match {
          case None =>
            flatten(src).map {
              case Vector() => Val.I(0, Type.U0)
              case Vector(r) => Val.R(r)
              case rs => Val.Struct(rs.map(Val.R))
            }.map(FlowControl.Return).map((Vector.empty, _))
          case Some(sret) =>
            for {
              stores <- txInstruction(Inst.Do(Op.Store(Val.R(sret), Val.I(0, Type.I64), Val.R(src))))
              r <- genReg(Type.Ptr)
              copy = Inst.Move(r, Op.Copy(Val.R(sret)))
            } yield (stores :+ copy, FlowControl.Return(Val.R(r)))
        }
      } yield res
  }

}
