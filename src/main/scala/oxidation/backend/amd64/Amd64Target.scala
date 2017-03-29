package oxidation
package backend
package amd64

import Reg._
import cats._
import cats.data._
import cats.implicits._
import oxidation.backend.shared.RegisterAllocator

class Amd64Target { this: Output =>

  type S[A] = Writer[M, A]

  val S = new MonadWriter[S, M] {

    private val w = MonadWriter[S, M]

    override def writer[A](aw: (M, A)): S[A] = w.writer(aw)

    override def listen[A](fa: S[A]): S[(M, A)] = w.listen(fa)

    override def pass[A](fa: S[((M) => M, A)]): S[A] = w.pass(fa)

    override def pure[A](x: A): S[A] = w.pure(x)

    override def flatMap[A, B](fa: S[A])(f: (A) => S[B]): S[B] = w.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: (A) => S[Either[A, B]]): S[B] = w.tailRecM(a)(f)
  }

  val allocator: RegisterAllocator[RegLoc] =
    new RegisterAllocator[RegLoc](RegLoc.calleeSaved, RegLoc.callerSaved)

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

  def outputDefs(ds: Vector[ir.Def]): M = {
    text |+| ds.foldMap(outputDef)
  }

  def outputDef(d: ir.Def): M = d match {
    case ir.Def.ExternFun(name, _, _) =>
      extern(name)
    case ir.Def.Fun(name, _, _, _) =>
      val (precolours, Vector(fun @ ir.Def.Fun(_, _, _, blocks))) = Amd64BackendPass.txDef(d).run.runEmptyA.value
      val allocations = allocator.allocate(fun, precolours.toMap)
      val spills = allocations.collect {
        case (r, allocator.Spill) => r
      }.zipWithIndex.toMap
      val bindings = allocations.map {
        case (reg, allocator.R(r)) => reg -> Val.R(Reg(r, regSize(reg.typ)))
        case (r, allocator.Spill) => r -> Val.m(regSize(r.typ), RBP, -8 * spills(r))
      }
      val calleeSaved = allocations.values.collect {
        case allocator.R(l) if RegLoc.calleeSaved.contains(l) => l
      }.toList.distinct
      val requiredStackSpace = spills.size + 4 // 4 qwords of shadow space for called procedures
      val res: S[Unit] = blocks.traverse_ {
        case ir.Block(name, instructions, flow) =>
          for {
            _ <- S.tell(label(name))
            _ <- instructions.traverse(outputInstruction(_)(bindings))
            _ <- outputFlow(flow, requiredStackSpace, calleeSaved)(bindings)
          } yield ()
      }
      val m = res.written
      Vector(
        global(name),
        label(name),
        prologue(requiredStackSpace, calleeSaved),
        m
      ).combineAll
  }

  def move(dest: Val, src: Val): M =
    if(dest == src) M.empty else mov(dest, src)

  def outputInstruction(i: ir.Inst)(implicit bindings: Map[ir.Register, Val]): S[Unit] = i match {
    case ir.Inst.Label(n) => S.tell(label(n))

    case ir.Inst.Do(ir.Op.Copy(_)) => S.pure(())

    case ir.Inst.Do(ir.Op.Store(addr, offset, value)) =>
      assert(regSize(addr.typ) == RegSize.QWord)
      assert(regSize(offset.typ) == RegSize.QWord)
      S.tell(move(Val.m(regSize(value.typ), toVal(addr), toVal(offset)), toVal(value)))

    case ir.Inst.Eval(_, ir.Op.Call(ir.Val.G(fun, _), params)) =>
      if(params.drop(4).nonEmpty) throw new NotImplementedError("passing parameters on stack is not yet supported")
      S.tell(call(fun))

    case ir.Inst.Move(dest, op) => op match {
      case ir.Op.Widen(src) => (signedness(src.typ), regSize(dest.typ), regSize(src.typ)) match {
        case (_, a, b) if a == b => S.tell(mov(toVal(dest), toVal(src)))

        case (Unsigned, RegSize.QWord, RegSize.DWord) =>
          val Val.R(Reg(destLoc, _)) = toVal(dest)
          S.tell(mov(Reg(destLoc, RegSize.DWord), toVal(src)))

        case (Signed, RegSize.QWord, RegSize.DWord) =>
          S.tell(movsxd(toVal(dest), toVal(src)))

        case (Unsigned, _, _) => S.tell(movzx(toVal(dest), toVal(src)))
        case (Signed, _, _) => S.tell(movsx(toVal(dest), toVal(src)))
      }

      case ir.Op.Arith(op @ (InfixOp.Add | InfixOp.Sub), left, right) =>
        S.tell(Vector(
          mov(toVal(dest), toVal(left)),
          op match {
            case InfixOp.Add => add(toVal(dest), toVal(right))
            case InfixOp.Sub => sub(toVal(dest), toVal(right))
          }
        ).combineAll)
      case ir.Op.Arith(InfixOp.Div, _, right) => S.tell(div(toVal(right)))
      case ir.Op.Arith(InfixOp.Mul, _, right) => S.tell(mul(toVal(right)))
      case ir.Op.Arith(op @ (InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq | InfixOp.Eq), left, right) =>
          S.tell(Vector(
            cmp(toVal(left), toVal(right)),
            op match {
              case InfixOp.Lt => setl(toVal(dest))
              case InfixOp.Gt => setg(toVal(dest))
              case InfixOp.Geq => setge(toVal(dest))
              case InfixOp.Leq => setle(toVal(dest))
              case InfixOp.Eq => sete(toVal(dest))
            }
          ).combineAll)

      case ir.Op.Copy(src) => S.tell(move(toVal(dest), toVal(src)))
      case ir.Op.Garbled => S.pure(())

      case ir.Op.Load(addr, off) =>
        assert(regSize(addr.typ) == RegSize.QWord)
        assert(regSize(off.typ) == RegSize.QWord)
        S.tell(
          mov(toVal(dest), Val.m(regSize(dest.typ), toVal(addr), toVal(off)))
        )
    }
  }

  def outputFlow(f: ir.FlowControl, localCount: Int, savedRegs: List[RegLoc])(implicit bindings: Map[ir.Register, Val]): S[Unit] = f match {
    case ir.FlowControl.Return(r) =>
      S.tell(Vector(
        move(Reg(RegLoc.A, regSize(r.typ)), toVal(r)),
        epilogue(localCount, savedRegs),
        ret
      ).combineAll)
    case ir.FlowControl.Goto(n) => S.tell(jmp(n))
    case ir.FlowControl.Branch(cond, ifTrue, ifFalse) =>
      S.tell(Vector(
        test(toVal(cond), toVal(cond)),
        jnz(ifTrue),
        jmp(ifFalse)
      ).combineAll)
  }

  def prologue(localCount: Int, regsToSave: List[RegLoc]): M = Vector(
    push(RBP),
    mov(RBP, RSP),
    regsToSave.foldMap(l => push(Reg(l, RegSize.QWord))),
    if(localCount == 0) M.empty else sub(RSP, localCount * 8)
  ).combineAll

  def epilogue(localCount: Int, savedRegs: List[RegLoc]): M = Vector(
    if(localCount == 0) M.empty else add(RSP, localCount * 8),
    savedRegs.reverse.foldMap(l => pop(Reg(l, RegSize.QWord))),
    mov(RSP, RBP),
    pop(RBP)
  ).combineAll

  private def toVal(v: ir.Val)(implicit bindings: Map[ir.Register, Val]): Val = v match {
    case ir.Val.I(i, _) => Val.I(i)
    case ir.Val.R(r) => bindings(r)
  }
  private def toVal(r: ir.Register)(implicit bindings: Map[ir.Register, Val]): Val = bindings(r)

}
