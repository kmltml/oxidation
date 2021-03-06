package oxidation
package backend
package amd64

import Reg._
import cats._
import cats.data._
import cats.implicits._
import monocle.Lens
import monocle.function.{Field1, Field2}
import oxidation.backend.shared.{BlockLinearizationPass, RegisterAllocator}
import oxidation.codegen.Name
import oxidation.ir.ConstantPoolEntry

import scala.annotation.tailrec

class Amd64Target { this: Output =>

  private var _spillCount: Int = 0

  def spillCount = _spillCount

  type F[A] = Writer[M, A]

  val F = implicitly[MonadWriter[F, M]]

  final val EntryPointName: Name = Name.Global(List("?entry?"))

  final case class FunCtxt(bindings: Map[ir.Register, Val] = Map.empty,
                           constants: Map[ir.ConstantPoolEntry, Name] = Map.empty,
                           stack: StackLayout = StackLayout.empty,
                           floats: Map[ir.Val, Name] = Map.empty)

  def regSize(t: ir.Type): RegSize = t match {
    case ir.Type.U0 | ir.Type.U1 | ir.Type.U8 | ir.Type.I8 => RegSize.Byte
    case ir.Type.U16 | ir.Type.I16 => RegSize.Word
    case ir.Type.U32 | ir.Type.I32 | ir.Type.F32 => RegSize.DWord
    case ir.Type.U64 | ir.Type.I64 | ir.Type.Ptr | ir.Type.F64 => RegSize.QWord
  }

  def signedness(t: ir.Type): Signedness = t match {
    case ir.Type.Ptr | ir.Type.U8 | ir.Type.U16 | ir.Type.U32 | ir.Type.U64 => Unsigned
    case ir.Type.I8 | ir.Type.I16 | ir.Type.I32 | ir.Type.I64 => Signed
  }

  def stackAllocOffset(index: Int): Int =
    if(index >= 0 && index <= 3) 0x10 + index * 8 // shadow space allocated by caller
    else -8 - (index - 4)*8

  def outputConstants(cs: Map[ir.ConstantPoolEntry, Name]): M =
    data |+| cs.toVector.foldMap((outputConstant _).tupled)

  def outputConstant(c: ConstantPoolEntry, name: Name): M = c match {
    case ConstantPoolEntry.Str(v) => defstr(name, v)
  }

  def outputDefs(ds: Vector[ir.Def])(implicit constants: Map[ir.ConstantPoolEntry, Name]) : M = {
    val funDefs = ds.collect {
      case f: ir.Def.Fun => f
      case f: ir.Def.ExternFun => f
    }
    val valDefs = ds.collect {
      case v: ir.Def.TrivialVal => v
    }
    text |+| funDefs.foldMap(outputFunDef) |+| data |+| valDefs.foldMap(outputValDef)
  }

  object AllocationState {

    def state[S, A](lens: Lens[(FunCtxt, ir.Def.Fun), S])(f: S => (S, A)): State[(FunCtxt, ir.Def.Fun), A] =
      State(s => f(lens.get(s)).leftMap(lens.set(_)(s)))

    val ctxtLens = Field1.first[(FunCtxt, ir.Def.Fun), FunCtxt]

    def funState[A] = state[ir.Def.Fun, A](Field2.second) _

    def stackState[A] = state[StackLayout, A](ctxtLens composeLens Lens[FunCtxt, StackLayout](_.stack)(a => s => s.copy(stack = a))) _

    def bindingsState[A] = state[Map[ir.Register, Val], A](ctxtLens composeLens Lens[FunCtxt, Map[ir.Register, Val]](_.bindings)(a => s => s.copy(bindings = a))) _

    def allocAll[R](ls: List[R])(f: R => StackAlloc): State[(FunCtxt, ir.Def.Fun), List[(R, StackAlloc.Addr)]] =
      ls.traverse(r => stackState(s => s.alloc(f(r)).map(r -> _)))

  }

  private def allocateIntRegs(stackParams: List[ir.Register], precolours: Map[ir.Register, RegLoc]): State[(FunCtxt, ir.Def.Fun), Unit] = {
    val allocator: RegisterAllocator[RegLoc] =
      new RegisterAllocator[RegLoc](RegLoc.calleeSaved, RegLoc.callerSaved, IntSpillPass.SpillFillReg) {

        override def rebuildAfterSpill(fun: ir.Def.Fun, spilled: Set[ir.Register]): ir.Def.Fun =
          IntSpillPass.txDef(fun).runEmptyA.run(spilled).head.asInstanceOf[ir.Def.Fun]

        override def includeRegister(register: ir.Register): Boolean = {
          val passedOnStack = stackParams.contains(register)
          val correctType = register.typ match {
            case _: ir.Type.Arr | _: ir.Type.F => false
            case _: ir.Type.Integral => true
            case ir.Type.U0 | ir.Type.U1 | ir.Type.Ptr => true
            case _: ir.Type.Fun | _: ir.Type.Struct =>
              throw new Exception(s"type ${register.typ} should be eliminated before target (used in ${register.show})")
          }
          !passedOnStack && correctType
        }

      }

    import AllocationState._

    for {
      allocations <- funState(allocator.allocate(_, precolours).swap)
      calleeSaved = allocations.values.collect {
        case allocator.R(l) if RegLoc.calleeSaved.contains(l) => l
      }.toSet
      newCalleeSaved <- stackState(s => (s, calleeSaved -- s.allocs.collect { case (StackAlloc.SavedIntReg(r), _) => r }.toSet))
      calleeSavedAllocs <- allocAll(newCalleeSaved.toList)(StackAlloc.SavedIntReg)

      spills = allocations.collect {
        case (r, allocator.Spill) => r
      }.toList
      regular = allocations.collect {
        case (reg, allocator.R(r)) => reg -> Val.R(Reg(r, regSize(reg.typ)))
      }
      spillAllocs <- allocAll(spills)(_ => StackAlloc.Spill)
      spillVals <- spillAllocs.traverse { case (r, a) => stackState(s => (s, r -> s.offset(a).withSize(regSize(r.typ)))) }
      _ <- bindingsState(b => (b ++ spillVals ++ regular, ()))
    } yield ()
  }

  private def allocateFloatRegs(stackParams: List[ir.Register], precolours: Map[ir.Register, Xmm]): State[(FunCtxt, ir.Def.Fun), Unit] = {
    val allocator: RegisterAllocator[Xmm] =
      new RegisterAllocator[Xmm](Xmm.calleeSaved, Xmm.callerSaved, FloatSpillPass.SpillFillReg) {

        override def rebuildAfterSpill(fun: ir.Def.Fun, spilled: Set[ir.Register]): ir.Def.Fun =
          FloatSpillPass.txDef(fun).runEmptyA.run(spilled).head.asInstanceOf[ir.Def.Fun]

        override def includeRegister(register: ir.Register): Boolean = {
          val passedOnStack = stackParams.contains(register)
          val correctType = register.typ match {
            case _: ir.Type.F => true
            case _: ir.Type.Arr | _: ir.Type.Integral | ir.Type.U0 | ir.Type.U1 | ir.Type.Ptr => false
            case _: ir.Type.Fun | _: ir.Type.Struct =>
              throw new Exception(s"type ${register.typ} should be eliminated before target (used in ${register.show})")
          }
          !passedOnStack && correctType
        }

      }

    import AllocationState._

    for {
      allocations <- funState(allocator.allocate(_, precolours).swap)
      calleeSaved = allocations.values.collect {
        case allocator.R(l) if RegLoc.calleeSaved.contains(l) => l
      }.toSet
      newCalleeSaved <- stackState(s => (s, s.allocs.collect { case (StackAlloc.SavedXmmReg(r), _) => r }.toSet -- calleeSaved))
      calleeSavedAllocs <- allocAll(newCalleeSaved.toList)(StackAlloc.SavedXmmReg)

      spills = allocations.collect {
        case (r, allocator.Spill) => r
      }.toList
      regular = allocations.collect {
        case (reg, allocator.R(r)) => reg -> Val.F(r)
      }
      spillAllocs <- allocAll(spills)(_ => StackAlloc.Spill)
      spillVals <- spillAllocs.traverse { case (r, a) => stackState(s => (s, r -> s.offset(a).withSize(regSize(r.typ)))) }
      _ <- bindingsState(b => (b ++ spillVals ++ regular, ()))
    } yield ()

  }

  private def allocateArrays: State[(FunCtxt, ir.Def.Fun), Unit] = {
    import AllocationState._

    for {
      arrayRegs <- funState(f => (f, f.body.flatMap(_.reads.collect {
        case r @ ir.Register(_, _, a: ir.Type.Arr) => (r, a)
      }).distinct))
      arrayAllocs <- allocAll(arrayRegs.toList) { case (_, t) => StackAlloc.Array(t) }

      arrayBindings <- arrayAllocs.traverse { case ((r, _), a) => stackState(s => (s, r -> s.offset(a))) }
      _ <- bindingsState(b => (b ++ arrayBindings, ()))
    } yield ()
  }

  def outputFunDef(d: ir.Def)(implicit constants: Map[ir.ConstantPoolEntry, Name]): M = d match {
    case ir.Def.ExternFun(name, _, _) =>
      extern(name)
    case ir.Def.Fun(name, _, _, _, _) =>
      val (precolours, Vector(unallocatedFun: ir.Def.Fun)) = Amd64BackendPass.txDef(d).run.runA(Amd64BackendPass.St()).value

      val stackParams = unallocatedFun.params.drop(4)

      val allocS = for {
        _ <- allocateIntRegs(stackParams, precolours.int.toMap)
        _ <- allocateFloatRegs(stackParams, precolours.float.toMap)
        _ <- allocateArrays
        stackParamBindings = stackParams.zipWithIndex.map {
          case (r, i) => r -> Val.m(Some(regSize(r.typ)), RBP, 0x30 + i * 8)
        }
        _ <- AllocationState.bindingsState(b => (b ++ stackParamBindings, ()))
        _ <- AllocationState.funState(f => (BlockLinearizationPass.txDef(f).head.asInstanceOf[ir.Def.Fun], ()))
      } yield ()
      val (ctxt, allocatedFun) = allocS.runS((FunCtxt(constants = constants), unallocatedFun)).value

      val floatVals = (for {
        block <- allocatedFun.body
        instr <- block.instructions
        v @ (ir.Val.F32(_) | ir.Val.F64(_)) <- instr.vals
      } yield v).distinct
      val floatNames = floatVals.zipWithIndex.map {
        case (v, i) => v -> Name.Local("$FL", i)
      }.toMap

      implicit val _ctxt = ctxt.copy(floats = floatNames, stack = ctxt.stack.align(2))

      val res: M = allocatedFun.body.foldMap {
        case ir.Block(_, instructions, _) =>
          @tailrec
          def go(acc: M, is: Vector[ir.Inst]): M =
            if(is.isEmpty) {
              acc
            } else {
              val (m, rest) = outputInstructions(is)
              go(acc |+| m, rest)
            }
          go(M.empty, instructions)
      }
      val floatM = floatNames.map {
        case (ir.Val.F32(f), n) => dd(n, java.lang.Float.floatToRawIntBits(f))
        case (ir.Val.F64(d), n) => dq(n, java.lang.Double.doubleToRawLongBits(d))
      }.toVector.combineAll

      _spillCount += _ctxt.stack.allocs.count {
        case (StackAlloc.Spill, _) => true
        case _ => false
      }

      Vector(
        global(name),
        label(name),
        prologue,
        res,
        floatM
      ).combineAll
  }

  def outputValDef(d: ir.Def)(implicit constants: Map[ir.ConstantPoolEntry, Name]): M = d match {
    case ir.Def.TrivialVal(n, v, _) => v match {
      case ir.Val.I(i, t: ir.Type.Num) =>
        t.size match {
          case 1 => db(n, i.toByte)
          case 2 => dw(n, i.toShort)
          case 4 => dd(n, i.toInt)
          case 8 => dq(n, i)
        }
      case v => db(n, v.representation: _*)
    }
  }

  def outputExtraDefs: M = Vector(
    global(EntryPointName),
    extern(Name.Global(List("exit"))),
    extern(Name.Global(List("GetCommandLineW"))),
    extern(Name.Global(List("CommandLineToArgvW"))),

    label(EntryPointName),
    sub(RSP, 32 + 8),

    call(Val.L(Name.Global(List("GetCommandLineW")))),

    mov(RCX, RAX),
    lea(RDX, Val.m(None, RSP, 32)),
    call(Val.L(Name.Global(List("CommandLineToArgvW")))),

    mov(RCX, Val.m(None, RSP, 32)),
    mov(RDX, RAX),
    call(Val.L(Name.Global(List("main")))),

    mov(RCX, RAX),
    call(Val.L(Name.Global(List("exit"))))

  ).combineAll

  def move(dest: Val, src: ir.Val)(implicit ctxt: FunCtxt): M =
    if(dest == toVal(src)) M.empty else (dest, src.typ) match {
      case (_, _: ir.Type.Integral | ir.Type.U1 | ir.Type.Ptr) => mov(dest, toVal(src))
      case (Val.F(_) | _: Val.M, ir.Type.F32) => movss(dest, toVal(src))
      case (Val.F(_) | _: Val.M, ir.Type.F64) => movsd(dest, toVal(src))
      case (Val.R(_), ir.Type.F32) => movd(dest, toVal(src))
      case (Val.R(_), ir.Type.F64) => movq(dest, toVal(src))
    }

  def move(dest: ir.Register, src: Val)(implicit ctxt: FunCtxt): M =
    if(toVal(dest) == src) M.empty else (toVal(dest), dest.typ) match {
      case (d, _: ir.Type.Integral | ir.Type.U1 | ir.Type.Ptr) => mov(d, src)
      case (d: Val.F, ir.Type.F32) => movss(d, src)
      case (d: Val.F, ir.Type.F64) => movsd(d, src)
      case (d: Val.R, ir.Type.F32) => movd(d, src)
      case (d: Val.R, ir.Type.F64) => movq(d, src)
    }

  def branch(cc: ConditionCode, ifTrue: Name, ifFalse: Name, nextLabel: Name): M =
    if(nextLabel == ifTrue)
      jcc(!cc, ifFalse)
    else if(nextLabel == ifFalse)
      jcc(cc, ifTrue)
    else
      jcc(cc, ifTrue) |+| jmp(ifFalse)

  def outputInstructions(is: Vector[ir.Inst])(implicit ctxt: FunCtxt): (M, Vector[ir.Inst]) = is match {

    case ir.Inst.Flow(ir.FlowControl.Branch(ir.Val.I(cond, ir.Type.U1), ifTrue, ifFalse)) +:
         rest =>
      val target = if(cond == 0) ifFalse else ifTrue
      outputInstructions(ir.Inst.Flow(ir.FlowControl.Goto(target)) +: rest)


    case ir.Inst.Flow(ir.FlowControl.Branch(cond, ifTrue, ifFalse)) +:
         ir.Inst.Label(lbl) +: rest =>

      val m = cmp(toVal(cond), 0) |+|
              branch(ConditionCode.NotEqual, ifTrue, ifFalse, lbl) |+|
              label(lbl)
      (m, rest)

    case ir.Inst.Move(flag, ir.Op.Binary(InfixOp.Comp(op), ir.Val(l, _: ir.Type.Integral | ir.Type.Ptr), r)) +:
         ir.Inst.Flow(ir.FlowControl.Branch(ir.Val.R(cond), ifTrue, ifFalse)) +:
         ir.Inst.Label(lbl) +: rest
         if flag == cond =>

      val cc = op match {
        case InfixOp.Eq => ConditionCode.Equal
        case InfixOp.Neq => ConditionCode.NotEqual
        case InfixOp.Lt => ConditionCode.Less
        case InfixOp.Leq => ConditionCode.LessOrEqual
        case InfixOp.Gt => ConditionCode.Greater
        case InfixOp.Geq => ConditionCode.GreaterOrEqual
      }

      val jumps = branch(cc, ifTrue, ifFalse, lbl)

      val m = cmp(toVal(l), toVal(r)) |+|
              setcc(cc, toVal(flag)) |+|
              jumps |+|
              label(lbl)
      (m, rest)

    case i +: rest => (outputInstruction(i), rest)
  }

  def outputInstruction(i: ir.Inst)(implicit ctxt: FunCtxt): M = i match {
    case ir.Inst.Label(n) => label(n)

    case ir.Inst.Flow(f) => outputFlow(f)

    case ir.Inst.Do(ir.Op.Copy(_)) => M.empty

    case ir.Inst.Move(_, ir.Op.Copy(ir.Val.UArr(_))) => M.empty

    case ir.Inst.Do(ir.Op.Store(addr, offset, value)) =>
      assert(regSize(addr.typ) == RegSize.QWord)
      assert(regSize(offset.typ) == RegSize.QWord)
      move(Val.m(Some(regSize(value.typ)), toVal(addr), toVal(offset)), value)

    case ir.Inst.Do(ir.Op.ArrStore(arr, index, value)) =>
      val arrM = toVal(arr).asInstanceOf[Val.M]
      val elemSize = value.typ.size
      val dest = toVal(index) match {
        case Val.I(i) => arrM + Val.m(Some(regSize(value.typ)), i * elemSize)
        case Val.R(r) => arrM + Val.m(Some(regSize(value.typ)), r * elemSize)
      }
      mov(dest, toVal(value))


    case ir.Inst.Eval(_, ir.Op.Call(fun, params)) =>
      val stackParams = params.drop(4)
      val alignedStackParamsSize = (stackParams.size * 8 + 15) & ~15
      Vector(
        if(stackParams.nonEmpty) sub(RSP, alignedStackParamsSize) else M.empty,
        stackParams.zipWithIndex.foldMap {
          case (r, i) =>
            val dest = Val.m(Some(regSize(r.typ)), RSP, 8 * (4 + i))
            r.typ match {
              case ir.Type.F32 =>
                r match {
                  case ir.Val.F32(f) => mov(dest, Val.i(java.lang.Float.floatToRawIntBits(f)))
                  case _ => movss(dest, toVal(r))
                }
              case ir.Type.F64 =>
                r match {
                  case ir.Val.F64(f) => mov(dest, Val.i(java.lang.Double.doubleToRawLongBits(f)))
                  case _ => movsd(dest, toVal(r))
                }
              case _ => mov(dest, toVal(r))
            }
        },
        call(toVal(fun)),
        if(stackParams.nonEmpty) add(RSP, alignedStackParamsSize) else M.empty
      ).combineAll

    case ir.Inst.Move(dest, op) => op match {
      case ir.Op.Widen(src) => (signedness(src.typ), regSize(dest.typ), regSize(src.typ)) match {
        case (_, a, b) if a == b => mov(toVal(dest), toVal(src))

        case (Unsigned, RegSize.QWord, RegSize.DWord) =>
          val Val.R(Reg(destLoc, _)) = toVal(dest)
          move(Reg(destLoc, RegSize.DWord), src)

        case (Signed, RegSize.QWord, RegSize.DWord) =>
          movsxd(toVal(dest), toVal(src))

        case (Unsigned, _, _) => movzx(toVal(dest), toVal(src))
        case (Signed, _, _) => movsx(toVal(dest), toVal(src))
      }

      case ir.Op.Convert(v, _: ir.Type.I) => v.typ match {
        case ir.Type.F32 => cvttss2si(toVal(dest), toVal(v))
        case ir.Type.F64 => cvttsd2si(toVal(dest), toVal(v))
      }
      case ir.Op.Convert(v, t: ir.Type.F) => t match {
        case ir.Type.F32 => cvtsi2ss(toVal(dest), toVal(v))
        case ir.Type.F64 => cvtsi2sd(toVal(dest), toVal(v))
      }

      case ir.Op.Reinterpret(src, _) =>
        mov(toVal(dest), toVal(src))

      case ir.Op.Sqrt(src) =>
        src.typ match {
          case ir.Type.F32 => sqrtss(toVal(dest), toVal(src))
          case ir.Type.F64 => sqrtsd(toVal(dest), toVal(src))
        }

      case ir.Op.Trim(src) =>
        toVal(src) match {
          case Val.R(Reg(loc, _)) =>
            mov(toVal(dest), Val.R(Reg(loc, regSize(dest.typ))))
          case v => mov(toVal(dest), v.withSize(regSize(dest.typ)))
        }

      case ir.Op.Elem(arr, index) =>
        val arrM = toVal(arr).asInstanceOf[Val.M]
        val elemSize = dest.typ.size
        val src = toVal(index) match {
          case Val.I(i) => arrM + Val.m(Some(regSize(dest.typ)), i * elemSize)
          case Val.R(r) => arrM + Val.m(Some(regSize(dest.typ)), r * elemSize)
        }
        mov(toVal(dest), src)

      case ir.Op.Unary(PrefixOp.Not, src) =>
        move(toVal(dest), src) |+|
        xor(toVal(dest), 1)

      case ir.Op.Unary(PrefixOp.Neg, src) =>
        move(toVal(dest), src) |+|
        neg(toVal(dest))

      case ir.Op.Binary(op, left, right) => left.typ match {
        case _: ir.Type.Integral | ir.Type.U1 | ir.Type.Ptr => op match {
          case InfixOp.Add | InfixOp.Sub | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Xor =>
            move(toVal(dest), left) |+|
            (op match {
              case InfixOp.Add => add(toVal(dest), toVal(right))
              case InfixOp.Sub => sub(toVal(dest), toVal(right))
              case InfixOp.BitAnd => and(toVal(dest), toVal(right))
              case InfixOp.BitOr => or(toVal(dest), toVal(right))
              case InfixOp.Xor => xor(toVal(dest), toVal(right))
            })
          case InfixOp.Shl | InfixOp.Shr =>
            move(toVal(dest), left) |+|
            (op match {
              case InfixOp.Shl => shl(toVal(dest), toVal(right).withSize(RegSize.Byte))
              case InfixOp.Shr => signedness(dest.typ) match {
                case Signed   => sar(toVal(dest), toVal(right).withSize(RegSize.Byte))
                case Unsigned => shr(toVal(dest), toVal(right).withSize(RegSize.Byte))
              }
            })

          case InfixOp.Div => div(toVal(right))
          case InfixOp.Mod => div(toVal(right))
          case InfixOp.Mul => mul(toVal(right))
          case (InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq | InfixOp.Eq | InfixOp.Neq) =>
            cmp(toVal(left), toVal(right)) |+|
            (op match {
              case InfixOp.Lt => setl(toVal(dest))
              case InfixOp.Gt => setg(toVal(dest))
              case InfixOp.Geq => setge(toVal(dest))
              case InfixOp.Leq => setle(toVal(dest))
              case InfixOp.Eq => sete(toVal(dest))
              case InfixOp.Neq => setne(toVal(dest))
            })
        }
        case ir.Type.F32 => op match {
          case InfixOp.Add | InfixOp.Sub | InfixOp.Div | InfixOp.Mul =>
            move(toVal(dest), left) |+|
            (op match {
              case InfixOp.Add => addss(toVal(dest), toVal(right))
              case InfixOp.Sub => subss(toVal(dest), toVal(right))
              case InfixOp.Div => divss(toVal(dest), toVal(right))
              case InfixOp.Mul => mulss(toVal(dest), toVal(right))
            })

          case InfixOp.Eq | InfixOp.Neq =>
            (op match {
              case InfixOp.Eq => cmpeqss(toVal(left), toVal(right))
              case InfixOp.Neq => cmpneqss(toVal(left), toVal(right))
            }) |+|
            move(toVal(dest).withSize(RegSize.DWord), left) |+|
            neg(toVal(dest).withSize(RegSize.DWord))

          case InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq =>
            ucomiss(toVal(left), toVal(right)) |+|
            (op match {
              case InfixOp.Lt => setb(toVal(dest))
              case InfixOp.Leq => setbe(toVal(dest))
              case InfixOp.Gt => seta(toVal(dest))
              case InfixOp.Geq => setae(toVal(dest))
            })
        }
        case ir.Type.F64 => op match {
          case InfixOp.Add | InfixOp.Sub | InfixOp.Div | InfixOp.Mul =>
            move(toVal(dest), left) |+|
            (op match {
              case InfixOp.Add => addsd(toVal(dest), toVal(right))
              case InfixOp.Sub => subsd(toVal(dest), toVal(right))
              case InfixOp.Div => divsd(toVal(dest), toVal(right))
              case InfixOp.Mul => mulsd(toVal(dest), toVal(right))
            })
          case InfixOp.Eq | InfixOp.Neq =>
            (op match {
              case InfixOp.Eq => cmpeqsd(toVal(left), toVal(right))
              case InfixOp.Neq => cmpneqsd(toVal(left), toVal(right))
            }) |+|
            move(toVal(dest).withSize(RegSize.QWord), left) |+|
            neg(toVal(dest).withSize(RegSize.QWord))

          case InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq =>
            ucomisd(toVal(left), toVal(right)) |+|
            (op match {
              case InfixOp.Lt => setb(toVal(dest))
              case InfixOp.Leq => setbe(toVal(dest))
              case InfixOp.Gt => seta(toVal(dest))
              case InfixOp.Geq => setae(toVal(dest))
            })
        }

      }

      case ir.Op.Copy(src) => move(toVal(dest), src)

      case ir.Op.Garbled => M.empty

      case ir.Op.Load(addr, off) =>
        assert(regSize(addr.typ) == RegSize.QWord)
        assert(regSize(off.typ) == RegSize.QWord)
        move(dest, Val.m(Some(regSize(dest.typ)), toVal(addr), toVal(off)))

      case ir.Op.Stackalloc(size) =>
        val allocSize = (size + 15) & ~15 // align to 16 bytes
        assert(allocSize >= size && allocSize % 8 == 0)

        sub(RSP, allocSize) |+|
        lea(toVal(dest), Val.m(None, RSP, 32))

    }
  }

  def outputFlow(f: ir.FlowControl)(implicit ctxt: FunCtxt): M = f match {
    case ir.FlowControl.Return(r) =>
      epilogue |+| ret

    case ir.FlowControl.Goto(n) => jmp(n)
    case ir.FlowControl.Branch(cond, ifTrue, ifFalse) =>
        cmp(toVal(cond), 0) |+|
        jnz(ifTrue) |+|
        jmp(ifFalse)

    case ir.FlowControl.Unreachable => M.empty // TODO error out somehow?
  }

  def prologue(implicit ctxt: FunCtxt): M = {
    val regStores = ctxt.stack.allocs.collect {
      case (StackAlloc.SavedIntReg(r), a) => mov(ctxt.stack.offset(a).withSize(RegSize.QWord), Reg(r, RegSize.QWord))
      case (StackAlloc.SavedXmmReg(r), a) => movups(ctxt.stack.offset(a), r)
    }
    Vector(
      push(RBP),
      mov(RBP, RSP),
      regStores.combineAll,
      sub(RSP, (ctxt.stack.mainSize + 4) * 8)
    ).combineAll
  }

  def epilogue(implicit ctxt: FunCtxt): M = {
    val regRestores = ctxt.stack.allocs.collect {
      case (StackAlloc.SavedIntReg(r), a) => mov(Reg(r, RegSize.QWord), ctxt.stack.offset(a).withSize(RegSize.QWord))
      case (StackAlloc.SavedXmmReg(r), a) => movups(r, ctxt.stack.offset(a))
    }
    Vector(
      regRestores.combineAll,
      mov(RSP, RBP),
      pop(RBP)
    ).combineAll
  }

  private def toVal(v: ir.Val)(implicit ctxt: FunCtxt): Val = v match {
    case ir.Val.I(i, _) => Val.I(i)
    case ir.Val.R(r) => ctxt.bindings(r)
    case ir.Val.Const(entry, _) => Val.L(ctxt.constants(entry))
    case ir.Val.GlobalAddr(n) => Val.L(n)
    case ir.Val.G(n, _) => Val.L(n)
    case f : ir.Val.F32 => Val.m(None, ctxt.floats(f))
    case f : ir.Val.F64 => Val.m(None, ctxt.floats(f))
  }
  private def toVal(r: ir.Register)(implicit ctxt: FunCtxt): Val = ctxt.bindings(r)

}
