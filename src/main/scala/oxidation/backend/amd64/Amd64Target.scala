package oxidation
package backend
package amd64

import Reg._

import cats._
import cats.data._
import cats.implicits._

import codegen.ir

class Amd64Target { this: Output =>

  type S[A] = WriterT[State[RegisterBindings, ?], M, A]

  val S = new MonadWriter[S, M] with MonadState[S, RegisterBindings] {

    private val w = MonadWriter[S, M]
    private val s = MonadState[State[RegisterBindings, ?], RegisterBindings]

    override def writer[A](aw: (M, A)): S[A] = w.writer(aw)

    override def listen[A](fa: S[A]): S[(M, A)] = w.listen(fa)

    override def pass[A](fa: S[((M) => M, A)]): S[A] = w.pass(fa)

    override def get: S[RegisterBindings] = WriterT.lift(s.get)

    override def set(ss: RegisterBindings): S[Unit] = WriterT.lift(s.set(ss))

    override def pure[A](x: A): S[A] = w.pure(x)

    override def flatMap[A, B](fa: S[A])(f: (A) => S[B]): S[B] = w.flatMap(fa)(f)

    override def tailRecM[A, B](a: A)(f: (A) => S[Either[A, B]]): S[B] = w.tailRecM(a)(f)
  }

  implicit def monoidMonadMonoid[F[_], M](implicit F: Monad[F], M: Monoid[M]): Monoid[F[M]] = new Monoid[F[M]] {

    override def empty: F[M] = F.pure(M.empty)

    override def combine(x: F[M], y: F[M]): F[M] = for(xx <- x; yy <- y) yield xx |+| yy
  }

  def outputDef(d: ir.Def): M = d match {
    case ir.Def.Fun(name, params, _, blocks) =>
      val res: S[Unit] = blocks.traverse_ {
        case ir.Block(name, instructions, flow) =>
          for {
            _ <- S.tell(label(name))
            _ <- instructions.traverse(outputInstruction)
            _ <- outputFlow(flow)
          } yield ()
      }
      val paramBindings = params.zipWithIndex.map {
        case (r, 0) => r -> Val.R(RCX)
        case (r, 1) => r -> Val.R(RDX)
        case (r, 2) => r -> Val.R(R8)
        case (r, 3) => r -> Val.R(R9)
        case (r, i) => r -> Val.m(RBP, 8 * i)
      }.toMap
      val freeLocations = List(RCX, RDX, R8, R9, RAX, R10, R11, R12, R13, R14, R15, RDI, RSI, RBX).map(Val.R) diff paramBindings.values.toSeq
      val (bindings, (m, _)) = res.run.run(RegisterBindings(locations = paramBindings, freeLocations = freeLocations)).value
      Vector(
        label(name),
        prologue(bindings.requiredStackSpace),
        m
      ).combineAll
  }

  def outputInstruction(i: ir.Inst): S[Unit] = i match {
    case ir.Inst.Label(n) => S.tell(label(n))
    case ir.Inst.Eval(dest, op) => op match {
      case ir.Op.Arith(InfixOp.Add, left, right) => dest match {
        case None => S.pure(())
        case Some(r) => for {
          lv <- find(left)
          rv <- find(right)
          res <- reg(r)
          _ <- S.tell(Vector(
            mov(res, lv),
            add(res, rv)
          ).combineAll)
        } yield ()
      }
    }
  }

  def outputFlow(f: ir.FlowControl): S[Unit] = f match {
    case ir.FlowControl.Return(r) => for {
      rval <- find(r)
      _ <- S.tell(Vector(
        mov(RAX, rval),
        epilogue,
        ret
      ).combineAll)
    } yield ()
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

  private def find(v: ir.Val): S[Val] = v match {
    case ir.Val.I(i) => S.pure(Val.I(i))
    case ir.Val.R(r) => reg(r)
  }

  private def reg(r: ir.Register): S[Val] = for {
    bind <- S.inspect(_.locations.get(r))
    reg <- bind.map(S.pure).getOrElse(bindFreeReg(r))
  } yield reg

  private def bindFreeReg(r: ir.Register): S[Val] = for {
    freeLocations <- S.inspect(_.freeLocations)
    reg <- freeLocations.headOption.map(S.pure)
      .getOrElse(allocateReg(r))
  } yield reg

  private def allocateReg(r: ir.Register): S[Val] = for {
    index <- S.inspect(_.requiredStackSpace)
    loc = Val.m(RBP, -8 * (index + 1))
    _ <- S.modify(s => s.copy(requiredStackSpace = index + 1,
                              locations = s.locations.updated(r, loc))) // [RBP] = stored RBP, so first local is [RBP - 8]
  } yield loc: Val

}
