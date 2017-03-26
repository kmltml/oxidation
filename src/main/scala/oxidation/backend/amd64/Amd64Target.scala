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

  val allocator: RegisterAllocator[Reg] =
    new RegisterAllocator[Reg](Reg.calleeSaved, Reg.callerSaved)

  def outputDef(d: ir.Def): M = d match {
    case ir.Def.Fun(name, _, _, _) =>
      val (precolours, Vector(fun @ ir.Def.Fun(_, _, _, blocks))) = Amd64BackendPass.txDef(d).run.runEmptyA.value
      val allocations = allocator.allocate(fun, precolours.toMap)
      val spills = allocations.collect {
        case (r, allocator.Spill) => r
      }.zipWithIndex.toMap
      val bindings = allocations.map {
        case (reg, allocator.R(r)) => reg -> Val.R(r)
        case (r, allocator.Spill) => r -> Val.m(RBP, -8 * spills(r))
      }
      val requiredStackSpace = spills.size * 8
      val res: S[Unit] = blocks.traverse_ {
        case ir.Block(name, instructions, flow) =>
          for {
            _ <- S.tell(label(name))
            _ <- instructions.traverse(outputInstruction(_)(bindings))
            _ <- outputFlow(flow)(bindings)
          } yield ()
      }
      val m = res.written
      Vector(
        label(name),
        prologue(requiredStackSpace),
        m
      ).combineAll
  }

  def move(dest: Val, src: Val): M =
    if(dest == src) M.empty else mov(dest, src)

  def outputInstruction(i: ir.Inst)(implicit bindings: Map[ir.Register, Val]): S[Unit] = i match {
    case ir.Inst.Label(n) => S.tell(label(n))

    case ir.Inst.Do(ir.Op.Copy(_)) => S.pure(())

    case ir.Inst.Move(dest, op) => op match {
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

    }
  }

  def outputFlow(f: ir.FlowControl)(implicit bindings: Map[ir.Register, Val]): S[Unit] = f match {
    case ir.FlowControl.Return(r) =>
      S.tell(Vector(
        move(RAX, toVal(r)),
        epilogue,
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

  def prologue(localCount: Int): M = Vector(
    push(RBP),
    mov(RBP, RSP),
    sub(RSP, localCount * 8)
  ).combineAll

  def epilogue: M = Vector(
    mov(RSP, RBP),
    pop(RBP)
  ).combineAll

  private def toVal(v: ir.Val)(implicit bindings: Map[ir.Register, Val]): Val = v match {
    case ir.Val.I(i, _) => Val.I(i)
    case ir.Val.R(r) => bindings(r)
  }
  private def toVal(r: ir.Register)(implicit bindings: Map[ir.Register, Val]): Val = bindings(r)

}
