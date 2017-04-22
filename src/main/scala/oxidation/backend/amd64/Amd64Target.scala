package oxidation
package backend
package amd64

import Reg._
import cats._
import cats.data._
import cats.implicits._
import oxidation.backend.shared.RegisterAllocator
import oxidation.codegen.Name
import oxidation.ir.{ConstantPoolEntry, Register}

class Amd64Target { this: Output =>

  type F[A] = Writer[M, A]

  val F = implicitly[MonadWriter[F, M]]

  final val EntryPointName: Name = Name.Global(List("?entry?"))

  val allocator: RegisterAllocator[RegLoc] =
    new RegisterAllocator[RegLoc](RegLoc.calleeSaved, RegLoc.callerSaved) {

      override def rebuildAfterSpill(fun: ir.Def.Fun, spilled: Set[ir.Register]): ir.Def.Fun =
        RegisterSpillPass.txDef(fun).runEmptyA.run(spilled).head.asInstanceOf[ir.Def.Fun]

      override def includeRegister(register: Register): Boolean = register.typ match {
        case _: ir.Type.Arr => false
        case _: ir.Type.Num => true
        case ir.Type.U0 | ir.Type.U1 | ir.Type.Ptr => true
        case _: ir.Type.Fun | _: ir.Type.Struct => throw new Exception(s"type ${register.typ} should be eliminated before target")
      }

    }

  def regSize(t: ir.Type): RegSize = t match {
    case ir.Type.U0 | ir.Type.U1 | ir.Type.U8 | ir.Type.I8 => RegSize.Byte
    case ir.Type.U16 | ir.Type.I16 => RegSize.Word
    case ir.Type.U32 | ir.Type.I32 => RegSize.DWord
    case ir.Type.U64 | ir.Type.I64 | ir.Type.Ptr => RegSize.QWord
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

  def outputDefs(ds: Vector[ir.Def])(implicit constants: Map[ir.ConstantPoolEntry, Name]) : M =
    text |+| ds.foldMap(outputDef)

  def outputDef(d: ir.Def)(implicit constants: Map[ir.ConstantPoolEntry, Name]): M = d match {
    case ir.Def.ExternFun(name, _, _) =>
      extern(name)
    case ir.Def.Fun(name, _, _, _, _) =>
      val (precolours, Vector(unallocatedFun: ir.Def.Fun)) = Amd64BackendPass.txDef(d).run.runA(Amd64BackendPass.St()).value
      val (allocations, fun) = allocator.allocate(unallocatedFun, precolours.toMap)
      val calleeSaved = allocations.values.collect {
        case allocator.R(l) if RegLoc.calleeSaved.contains(l) => l
      }.toList.distinct
      val spills = allocations.collect {
        case (r, allocator.Spill) => r
      }.zipWithIndex.toMap.mapValues(_ + calleeSaved.size)
      assert(fun.body.flatMap(_.reads).filterNot(allocations.contains).forall(!allocator.includeRegister(_)))
      val bindings = allocations.map {
        case (reg, allocator.R(r)) => reg -> Val.R(Reg(r, regSize(reg.typ)))
        case (r, allocator.Spill) => r -> Val.m(Some(regSize(r.typ)), RBP, stackAllocOffset(spills(r)))
      }
      val requiredStackSpace = ((calleeSaved.size + spills.size - 4) max 0) * 8

      val arrayRegs = fun.body.flatMap(_.reads.collect {
        case r @ ir.Register(_, _, a: ir.Type.Arr) => r
      }).distinct
      val arraySizes = arrayRegs.scanLeft(requiredStackSpace)(_ + _.typ.size) // TODO align
      val arrayAllocs = arrayRegs zip arraySizes

      val allocatedStackSpace = arraySizes.lastOption getOrElse requiredStackSpace

      val arrayBindings = arrayAllocs.map {
        case (r, i) => r -> Val.m(None, RBP, -(i + r.typ.size))
      }

      val allBindings = bindings ++ arrayBindings

      val res: F[Unit] = fun.body.traverse_ {
        case ir.Block(name, instructions, flow) =>
          for {
            _ <- F.tell(label(name))
            _ <- instructions.traverse(outputInstruction(_)(allBindings, constants))
            _ <- outputFlow(flow, calleeSaved)(bindings, constants)
          } yield ()
      }
      val m = res.written
      Vector(
        global(name),
        label(name),
        prologue(allocatedStackSpace, calleeSaved),
        m
      ).combineAll
  }

  def outputExtraDefs: M = Vector(
    global(EntryPointName),
    extern(Name.Global(List("exit"))),
    extern(Name.Global(List("GetCommandLineW"))),
    extern(Name.Global(List("CommandLineToArgvW"))),

    label(EntryPointName),
    sub(RSP, 32 + 8),

    call(Name.Global(List("GetCommandLineW"))),

    mov(RCX, RAX),
    lea(RDX, Val.m(None, RSP, 32)),
    call(Name.Global(List("CommandLineToArgvW"))),

    mov(RCX, Val.m(None, RSP, 32)),
    mov(RDX, RAX),
    call(Name.Global(List("main"))),

    mov(RCX, RAX),
    call(Name.Global(List("exit")))

  ).combineAll

  def move(dest: Val, src: Val): M =
    if(dest == src) M.empty else mov(dest, src)

  def outputInstruction(i: ir.Inst)(implicit bindings: Map[ir.Register, Val], constants: Map[ir.ConstantPoolEntry, Name]): F[Unit] = i match {
    case ir.Inst.Label(n) => F.tell(label(n))

    case ir.Inst.Do(ir.Op.Copy(_)) => F.pure(())

    case ir.Inst.Do(ir.Op.Store(addr, offset, value)) =>
      assert(regSize(addr.typ) == RegSize.QWord)
      assert(regSize(offset.typ) == RegSize.QWord)
      F.tell(move(Val.m(Some(regSize(value.typ)), toVal(addr), toVal(offset)), toVal(value)))

    case ir.Inst.Do(ir.Op.ArrStore(arr, index, value)) =>
      val arrM = toVal(arr).asInstanceOf[Val.M]
      val elemSize = value.typ.size
      val dest = toVal(index) match {
        case Val.I(i) => arrM + Val.m(Some(regSize(value.typ)), i * elemSize)
        case Val.R(r) => arrM + Val.m(Some(regSize(value.typ)), r * elemSize)
      }
      F.tell(mov(dest, toVal(value)))


    case ir.Inst.Eval(_, ir.Op.Call(ir.Val.G(fun, _), params)) =>
      if(params.drop(4).nonEmpty) throw new NotImplementedError("passing parameters on stack is not yet supported")
      F.tell(call(fun))

    case ir.Inst.Move(dest, op) => op match {
      case ir.Op.Widen(src) => (signedness(src.typ), regSize(dest.typ), regSize(src.typ)) match {
        case (_, a, b) if a == b => F.tell(mov(toVal(dest), toVal(src)))

        case (Unsigned, RegSize.QWord, RegSize.DWord) =>
          val Val.R(Reg(destLoc, _)) = toVal(dest)
          F.tell(move(Reg(destLoc, RegSize.DWord), toVal(src)))

        case (Signed, RegSize.QWord, RegSize.DWord) =>
          F.tell(movsxd(toVal(dest), toVal(src)))

        case (Unsigned, _, _) => F.tell(movzx(toVal(dest), toVal(src)))
        case (Signed, _, _) => F.tell(movsx(toVal(dest), toVal(src)))
      }

      case ir.Op.Trim(src) =>
        toVal(src) match {
          case Val.R(Reg(loc, _)) =>
            F.tell(move(toVal(dest), Val.R(Reg(loc, regSize(dest.typ)))))
          case v => F.tell(move(toVal(dest), v))
        }

      case ir.Op.Elem(arr, index) =>
        val arrM = toVal(arr).asInstanceOf[Val.M]
        val elemSize = dest.typ.size
        val src = toVal(index) match {
          case Val.I(i) => arrM + Val.m(Some(regSize(dest.typ)), i * elemSize)
          case Val.R(r) => arrM + Val.m(Some(regSize(dest.typ)), r * elemSize)
        }
        F.tell(move(toVal(dest), src))

      case ir.Op.Unary(PrefixOp.Not, src) =>
        F.tell(Vector(
          move(toVal(dest), toVal(src)),
          xor(toVal(dest), 1)
        ).combineAll)

      case ir.Op.Unary(PrefixOp.Neg, src) =>
        F.tell(Vector(
          move(toVal(dest), toVal(src)),
          neg(toVal(dest))
        ).combineAll)

      case ir.Op.Binary(op @ (InfixOp.Add | InfixOp.Sub | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Xor | InfixOp.Shl | InfixOp.Shr), left, right) =>
        F.tell(Vector(
          move(toVal(dest), toVal(left)),
          op match {
            case InfixOp.Add => add(toVal(dest), toVal(right))
            case InfixOp.Sub => sub(toVal(dest), toVal(right))
            case InfixOp.BitAnd => and(toVal(dest), toVal(right))
            case InfixOp.BitOr => or(toVal(dest), toVal(right))
            case InfixOp.Xor => xor(toVal(dest), toVal(right))
            case InfixOp.Shl => shl(toVal(dest), toVal(right))
            case InfixOp.Shr => signedness(dest.typ) match {
              case Signed   => sar(toVal(dest), toVal(right))
              case Unsigned => shr(toVal(dest), toVal(right))
            }
          }
        ).combineAll)
      case ir.Op.Binary(InfixOp.Div, _, right) => F.tell(div(toVal(right)))
      case ir.Op.Binary(InfixOp.Mod, _, right) => F.tell(div(toVal(right)))
      case ir.Op.Binary(InfixOp.Mul, _, right) => F.tell(mul(toVal(right)))
      case ir.Op.Binary(op @ (InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq | InfixOp.Eq | InfixOp.Neq), left, right) =>
          F.tell(Vector(
            cmp(toVal(left), toVal(right)),
            op match {
              case InfixOp.Lt => setl(toVal(dest))
              case InfixOp.Gt => setg(toVal(dest))
              case InfixOp.Geq => setge(toVal(dest))
              case InfixOp.Leq => setle(toVal(dest))
              case InfixOp.Eq => sete(toVal(dest))
              case InfixOp.Neq => setne(toVal(dest))
            }
          ).combineAll)

      case ir.Op.Copy(src) => F.tell(move(toVal(dest), toVal(src)))
      case ir.Op.Garbled => F.pure(())

      case ir.Op.Load(addr, off) =>
        assert(regSize(addr.typ) == RegSize.QWord)
        assert(regSize(off.typ) == RegSize.QWord)
        F.tell(
          mov(toVal(dest), Val.m(Some(regSize(dest.typ)), toVal(addr), toVal(off)))
        )

      case ir.Op.Stackalloc(size) =>
        val allocSize = (size + 7) & ~7 // align to 8 bytes
        assert(allocSize >= size && allocSize % 8 == 0)
        F.tell(Vector(
          sub(RSP, allocSize),
          lea(toVal(dest), Val.m(None, RSP, 32))
        ).combineAll)

      case ir.Op.Arr(None) =>
        F.pure(())
    }
  }

  def outputFlow(f: ir.FlowControl, savedRegs: List[RegLoc])(implicit bindings: Map[ir.Register, Val], constants: Map[ir.ConstantPoolEntry, Name]): F[Unit] = f match {
    case ir.FlowControl.Return(r) =>
      F.tell(Vector(
        epilogue(savedRegs),
        ret
      ).combineAll)
    case ir.FlowControl.Goto(n) => F.tell(jmp(n))
    case ir.FlowControl.Branch(cond, ifTrue, ifFalse) =>
      F.tell(Vector(
        cmp(toVal(cond), 0),
        jnz(ifTrue),
        jmp(ifFalse)
      ).combineAll)
  }

  def prologue(localCount: Int, regsToSave: List[RegLoc]): M = Vector(
    push(RBP),
    mov(RBP, RSP),
    regsToSave.zipWithIndex.foldMap { case (l, i) => mov(Val.m(Some(RegSize.QWord), RBP, stackAllocOffset(i)), Reg(l, RegSize.QWord)) },
    sub(RSP, localCount + 4*8)
  ).combineAll

  def epilogue(savedRegs: List[RegLoc]): M = Vector(
    savedRegs.zipWithIndex.foldMap { case (l, i) => mov(Reg(l, RegSize.QWord), Val.m(Some(RegSize.QWord), RBP, stackAllocOffset(i))) },
    mov(RSP, RBP),
    pop(RBP)
  ).combineAll

  private def toVal(v: ir.Val)(implicit bindings: Map[ir.Register, Val], constants: Map[ir.ConstantPoolEntry, Name]): Val = v match {
    case ir.Val.I(i, _) => Val.I(i)
    case ir.Val.R(r) => bindings(r)
    case ir.Val.Const(entry, _) => Val.L(constants(entry))
  }
  private def toVal(r: ir.Register)(implicit bindings: Map[ir.Register, Val]): Val = bindings(r)

}
