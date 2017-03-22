package oxidation
package backend
package amd64

import Reg._
import cats._
import cats.data._
import cats.implicits._
import codegen.ir
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

  val allocator: RegisterAllocator[Val] =
    new RegisterAllocator[Val](Reg.calleeSaved.map(Val.R), Reg.callerSaved.map(Val.R))

  def outputDef(d: ir.Def): M = d match {
    case ir.Def.Fun(name, params, _, blocks) =>
      val paramBindings = params.zipWithIndex.map {
        case (r, 0) => r -> Val.R(RCX)
        case (r, 1) => r -> Val.R(RDX)
        case (r, 2) => r -> Val.R(R8)
        case (r, 3) => r -> Val.R(R9)
        case (r, i) => r -> Val.m(RBP, 8 * i)
      }.toMap
      val allocations = allocator.allocate(blocks, paramBindings.mapValues(allocator.R))
      val bindings = allocations.mapValues {
        case allocator.R(r) => r
        case allocator.Stack(off) => Val.m(RBP, -8 * off)
      }
      val requiredStackSpace = allocations.values.collect {
        case allocator.Stack(off) => off
      }.toList.maximumOption.getOrElse(0) * 8
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

  def outputInstruction(i: ir.Inst)(implicit bindings: Map[ir.Register, Val]): S[Unit] = i match {
    case ir.Inst.Label(n) => S.tell(label(n))
    case ir.Inst.Eval(dest, op) => op match {
      case ir.Op.Arith(op @ (InfixOp.Add | InfixOp.Sub), left, right) => dest match {
        case None => S.pure(())
        case Some(r) =>
          S.tell(Vector(
            mov(toVal(r), toVal(left)),
            op match {
              case InfixOp.Add => add(toVal(r), toVal(right))
              case InfixOp.Add => sub(toVal(r), toVal(right))
            }
          ).combineAll)
      }
      case ir.Op.Arith(op @ (InfixOp.Div | InfixOp.Mod), left, right) => dest match {
        case None => S.pure(())
        case Some(r) =>
          val res = toVal(r)
          S.tell(Vector(
            res match { case Val.R(RAX) => M.empty; case _ => push(RAX) },
            res match { case Val.R(RDX) => M.empty; case _ => push(RDX) },
            res match { case Val.R(RDI) => M.empty; case _ => push(RDI) },
            mov(RDI, toVal(right)),
            mov(RDX, 0),
            mov(RAX, toVal(left)),
            div(RAX, RDI),
            mov(toVal(r), op match {
              case InfixOp.Div => RAX
              case InfixOp.Mod => RDX
            }),
            res match { case Val.R(RAX) => M.empty; case _ => pop(RAX) },
            res match { case Val.R(RDX) => M.empty; case _ => pop(RDX) },
            res match { case Val.R(RDI) => M.empty; case _ => pop(RDI) }
          ).combineAll)
      }
      case ir.Op.Arith(op @ (InfixOp.Lt | InfixOp.Gt | InfixOp.Geq | InfixOp.Leq | InfixOp.Eq), left, right) => dest match {
        case None => S.pure(())
        case Some(r) =>
          S.tell(Vector(
            cmp(toVal(left), toVal(right)),
            op match {
              case InfixOp.Lt => setl(toVal(r))
              case InfixOp.Gt => setg(toVal(r))
              case InfixOp.Geq => setge(toVal(r))
              case InfixOp.Leq => setle(toVal(r))
              case InfixOp.Eq => sete(toVal(r))
            }
          ).combineAll)
      }

      case ir.Op.Copy(src) => dest match {
        case None => S.pure(())
        case Some(r) => S.tell(mov(toVal(r), toVal(src)))
      }

    }
  }

  def outputFlow(f: ir.FlowControl)(implicit bindings: Map[ir.Register, Val]): S[Unit] = f match {
    case ir.FlowControl.Return(r) =>
      S.tell(Vector(
        mov(RAX, toVal(r)),
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
    case ir.Val.I(i) => Val.I(i)
    case ir.Val.R(r) => bindings(r)
  }
  private def toVal(r: ir.Register)(implicit bindings: Map[ir.Register, Val]): Val = bindings(r)

}
