package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._
import codegen.ir
import oxidation.codegen.ir.Register
import <->.EdgeSyntax

class RegisterAllocator[Reg](val calleeSavedRegs: List[Reg], val callerSavedRegs: List[Reg]) {

  sealed trait Alloc
  final case class R(r: Reg) extends Alloc
  final case class Stack(offset: Int) extends Alloc

  def allocate(blocks: Vector[ir.Block], preallocated: Map[ir.Register, Alloc]): Map[ir.Register, Alloc] = {
    // Placeholder no-op register allocation here
    val irRegs = for {
      block <- blocks
      inst <- block.instructions
      r <- inst match {
        case ir.Inst.Move(r, _) => Vector(r)
        case _ => Vector.empty
      }
    } yield r

    type S[A] = State[(List[Reg], Int), A]
    val S = MonadState[S, (List[Reg], Int)]

    val freeRegs: List[Reg] = (callerSavedRegs ++ calleeSavedRegs) diff preallocated.values.collect {
      case R(r) => r
    }.toList

    irRegs.traverse(r => for {
      free <- S.get
      reg <- free match {
        case (Nil, i) => S.set((Nil, i + 1)).as(Stack(i): Alloc)
        case (h :: t, i) => S.set((t, i)).as(R(h): Alloc)
      }
    } yield r -> reg).runA((freeRegs, 0)).value.toMap ++ preallocated
  }

  sealed trait Bound extends Product with Serializable
    { def location: Int; def register: ir.Register }
  final case class Start(location: Int, register: ir.Register) extends Bound
  final case class End(location: Int, register: ir.Register) extends Bound

  implicit val boundOrder: Ordering[Bound] = {
    case (Start(x, _), End(y, _)) if x == y => 1
    case (End(x, _), Start(y, _)) if x == y => -1
    case (a, b) => a.location - b.location
  }

  def buildInterferenceSubGraph(block: ir.Block, inputs: Set[ir.Register],
                                outputs: Set[ir.Register]): InterferenceGraph[ir.Register, Reg] = {
    val instrs = (block.instructions :+ ir.Inst.Flow(block.flow)).zipWithIndex
    val regs = block.instructions.flatMap(_.regs)
    val lifetimes = regs.map(r => r -> RegisterLifetime.lifetime(r, instrs, inputs, outputs)).toMap
    val sorted = lifetimes.flatMap {
      case (r, (Some(s), Some(e))) => Vector(Start(s, r), End(e, r))
      case (r, (Some(s), None)) => Vector(Start(s, r))
      case (r, (None, Some(e))) => Vector(End(e, r))
      case _ => Vector.empty
    }.toVector.sorted
    type F[A] = WriterT[State[Set[ir.Register], ?], Set[Edge[ir.Register]], A]
    val W = MonadWriter[F, Set[Edge[ir.Register]]]
    val S = new MonadState[F, Set[ir.Register]] {
      override def get: F[Set[Register]] = WriterT.lift(State.get)
      override def set(s: Set[Register]): F[Unit] = WriterT.lift(State.set(s))
      override def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = W.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: (A) => F[Either[A, B]]): F[B] = W.tailRecM(a)(f)
      override def pure[A](x: A): F[A] = W.pure(x)
    }
    val f: F[Unit] = sorted.traverse_ {
      case Start(_, r) => for {
        live <- S.get
        _ <- S.modify(_ + r)
        _ <- W.tell(live.map(r <-> _))
      } yield ()
      case End(_, r) => S.modify(_ - r)
    }
    val edges = f.written.runA(inputs).value
    InterferenceGraph(regs.toSet, Map.empty, edges)
  }

  def buildInterferenceGraph(fun: ir.Def.Fun): InterferenceGraph[ir.Register, Reg] = {
    val graph = FlowGraph(fun.body)
    val inputs = {
      val i = graph.blocks.mapValues(RegisterLifetime.inputs)
      val firstName = fun.body.head.name
      i.updated(firstName, i(firstName) ++ fun.params)
    }
    val outputs = graph.blocks.mapValues(b => RegisterLifetime.outputs(graph, b.name, inputs))
    val ghosts = graph.blocks.mapValues(b => RegisterLifetime.ghosts(graph, b.name, inputs, outputs))
    fun.body.foldMap { b =>
      buildInterferenceSubGraph(b, inputs(b.name) ++ ghosts(b.name), outputs(b.name) ++ ghosts(b.name))
    }
  }


}
