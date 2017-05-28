package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._

import oxidation.ir.RegisterNamespace

abstract class RegisterAllocator[Reg](val calleeSavedRegs: List[Reg], val callerSavedRegs: List[Reg]) {

  def rebuildAfterSpill(fun: ir.Def.Fun, spilled: Set[ir.Register]): ir.Def.Fun

  def includeRegister(register: ir.Register): Boolean

  sealed trait Alloc
  final case class R(r: Reg) extends Alloc
  case object Spill extends Alloc

  sealed trait Event extends Product with Serializable
    { def location: Int }
  final case class Write(location: Int, register: ir.Register, exclude: Set[ir.Register]) extends Event
  final case class End(location: Int, register: ir.Register) extends Event
  final case class Call(location: Int) extends Event

  implicit val eventOrder: Ordering[Event] = {
    case (Write(x, _, _), End(y, _)) if x == y => 1
    case (Write(x, _, _), Call(y)) if x == y => 1
    case (End(x, _), Write(y, _, _)) if x == y => -1
    case (Call(x), Write(y, _, _)) if x == y => -1
    case (a, b) => a.location - b.location
  }

  object VirtualReg extends RegisterNamespace {
    override def prefix: String = "vr"
  }

  val virtualRegs: Map[ir.Register, Reg] = callerSavedRegs.zipWithIndex.map {
    case (r, i) => ir.Register(VirtualReg, i, ir.Type.U0) -> r
  }.toMap

  def buildInterferenceSubGraph(block: ir.Block, inputs: Set[ir.Register],
                                outputs: Set[ir.Register]): InterferenceGraph[ir.Register, Reg] = {
    val instrs = (block.instructions :+ ir.Inst.Flow(block.flow)).zipWithIndex
    val regs = (block.instructions.flatMap(_.regs) ++ inputs ++ outputs).filter(includeRegister)
    val lifetimes = regs.map(r => r -> RegisterLifetime.lifetime(r, instrs, inputs, outputs)).toMap
    val calls = instrs.collect {
      case (ir.Inst.Eval(_, ir.Op.Call(_, _)), i) => Call(i)
    }
    val (writes, moves) = instrs.foldMap {
      case (ir.Inst.Move(r, ir.Op.Copy(ir.Val.R(src))), i) => (Vector(Write(i, r, Set(src))), Vector(r -> src))
      case (ir.Inst.Move(r, ir.Op.Widen(ir.Val.R(src))), i) => (Vector(Write(i, r, Set(src))), Vector(r -> src))
      case (ir.Inst.Move(r, _), i) => (Vector(Write(i, r, Set.empty)), Vector.empty)
      case _ => (Vector.empty, Vector.empty)
    } match {
      case (ws, mvs) => (ws.filter(w => includeRegister(w.register)), mvs.filter { case (a, b) => includeRegister(a) && includeRegister(b) })
    }
    val sorted = (lifetimes.flatMap {
      case (r, (_, Some(e))) => Vector(End(e, r))
      case _ => Vector.empty
    }.toVector ++ calls ++ writes).sorted
    type F[A] = WriterT[State[Set[ir.Register], ?], Set[(ir.Register, ir.Register)], A]
    val W = MonadWriter[F, Set[(ir.Register, ir.Register)]]
    val S = new MonadState[F, Set[ir.Register]] {
      override def get: F[Set[ir.Register]] = WriterT.lift(State.get)
      override def set(s: Set[ir.Register]): F[Unit] = WriterT.lift(State.set(s))
      override def flatMap[A, B](fa: F[A])(f: (A) => F[B]): F[B] = W.flatMap(fa)(f)
      override def tailRecM[A, B](a: A)(f: (A) => F[Either[A, B]]): F[B] = W.tailRecM(a)(f)
      override def pure[A](x: A): F[A] = W.pure(x)
    }
    assert(sorted.collect {
      case Write(_, r, _) => r
      case End(_, r) => r
    }.forall(includeRegister))
    val f: F[Unit] = sorted.traverse_ {
      case Write(_, r, ex) => for {
        live <- S.get
        _ <- S.modify(_ + r)
        _ <- W.tell((live -- ex - r).map(r -> _))
      } yield ()
      case End(_, r) => S.modify(_ - r)
      case Call(_) => for {
        live <- S.get
        _ <- W.tell(for (a <- live; b <- virtualRegs.keySet) yield a -> b)
      } yield ()
    }
    val edges = f.written.runA(inputs).value ++ inputs.subsets(2).map(_.toList match { case List(a, b) => a -> b })
    assert(edges.forall(t => (includeRegister(t._1) || virtualRegs.contains(t._1)) && (includeRegister(t._2) || virtualRegs.contains(t._2))))
    assert(moves.forall(t => includeRegister(t._1) && includeRegister(t._2)))
    val callVRegs = if(calls.isEmpty) Set() else virtualRegs.keySet
    InterferenceGraph.empty
      .withNodes(regs.toSet ++ callVRegs)
      .withInterferenceEdges(edges)
      .withPreferenceEdges(moves.toSet) ensuring (g => edges.forall { case (a, b) => g.interfering(a, b) })
  }

  def buildInterferenceGraph(fun: ir.Def.Fun): InterferenceGraph[ir.Register, Reg] = {
    val graph = FlowGraph(fun.body)
    val inputs = {
      val i = graph.blocks.mapValues(RegisterLifetime.inputs(_).filter(includeRegister))
      val firstName = fun.body.head.name
      i.updated(firstName, i(firstName) ++ fun.params.filter(includeRegister))
    }
    val outputs = graph.blocks.mapValues(b => RegisterLifetime.outputs(graph, b.name, inputs).filter(includeRegister))
    val ghosts = graph.blocks.mapValues(b => RegisterLifetime.ghosts(graph, b.name, inputs, outputs).filter(includeRegister))
    val interferenceGraph = fun.body.foldMap { b =>
      buildInterferenceSubGraph(b, inputs(b.name) ++ ghosts(b.name), outputs(b.name) ++ ghosts(b.name))
    }
    interferenceGraph.withColours(virtualRegs.filterKeys(interferenceGraph.nodes)) ensuring { g =>
      g.nodes.forall(r => includeRegister(r) || (virtualRegs contains r))
    }
  }

  val colourCount = calleeSavedRegs.size + callerSavedRegs.size
  val colours: Set[Reg] = calleeSavedRegs.toSet ++ callerSavedRegs.toSet

  type Graph = InterferenceGraph[Set[ir.Register], Reg]
  type RemovedNodes = List[(Set[ir.Register], Set[Set[ir.Register]])]

  def simplify(graph: Graph, removedNodes: RemovedNodes): (Graph, RemovedNodes) = {
    graph.nodes.find { n =>
      !graph.moveRelated(n) &&
      graph.degree(n) < colourCount &&
      !graph.precoloured(n)
    } match {
      case None => (graph, removedNodes)
      case Some(n) =>
        simplify(graph - n, (n, graph.neighbors(n)) :: removedNodes)
    }
  }

  def coalesce(graph: InterferenceGraph[Set[ir.Register], Reg]): InterferenceGraph[Set[ir.Register], Reg] = {
    graph.preferenceEdges.find {
      case (a, b) =>
        !graph.interfering(a, b) &&
        (graph.neighbors(a) ++ graph.neighbors(b)).count(graph.degree(_) >= colourCount) < colourCount &&
        !(graph.precoloured(a) && graph.precoloured(b)) &&
        !((graph.colours.get(a) orElse graph.colours.get(b)).exists(c => (graph.neighbors(a) ++ graph.neighbors(b)).flatMap(graph.colours.get).contains(c)))
        // TODO ^^ This is ugly, and not present in original paper, something should be done about this ^^
    } match {
      case None => graph
      case Some((a, b)) =>
        val removed = graph - a - b
        val newNode = a ++ b
        coalesce(removed
          .withNodes(Set(newNode))
          .withColours((graph.colours.get(a) orElse graph.colours.get(b)).map(r => Map(newNode -> r)) getOrElse Map.empty)
          .withInterferenceEdges((graph.neighbors(a) ++ graph.neighbors(b)).map(newNode -> _))
          .withPreferenceEdges((graph.moveNeighbors(a) ++ graph.moveNeighbors(b) diff Set(a, b)).map(newNode -> _)))
    }
  }

  def select(graph: Graph, removed: RemovedNodes): Graph = removed match {
    case (node, neighbors) :: rest =>
      assert(graph.nodes.forall(graph.precoloured), graph.nodes.filterNot(graph.precoloured).toString + " " + removed)
      val actualNeighbors = neighbors
        .map(n => graph.nodes.find(n subsetOf _).getOrElse(throw new AssertionError(s"neighbor $n not found in $graph, removed = $removed")))
      val neighborColors = actualNeighbors.map(graph.colours(_))
      // Simplify should only remove nodes in such way, that there will be a color available when re-inserting it
      // This failing to find a register would signify a bug in the simplify method, not here.
      // Also, prefer caller saved regs, to minimize register saving in prologue
      val colour = (callerSavedRegs.find(!neighborColors(_)) orElse calleeSavedRegs.find(!neighborColors(_))).get
      select(graph
        .withColours(Map(node -> colour))
        .withNodes(Set(node))
        .withInterferenceEdges(actualNeighbors.map(_ -> node)), rest)
    case Nil =>
      assert(graph.nodes.forall(graph.precoloured), graph.nodes.filterNot(graph.precoloured).toString + " " + removed)
      graph
  }

  def allocate(originalFun: ir.Def.Fun, precoloured: Map[ir.Register, Reg]): (Map[ir.Register, Alloc], ir.Def.Fun) = {

    def simplifyCoalesce(spilled: Set[ir.Register], fun: ir.Def.Fun, graph: Graph, removedNodes: RemovedNodes): Eval[(Map[ir.Register, Alloc], ir.Def.Fun)] = {
      val (g1, newRemoved) = simplify(graph, removedNodes)
      val g2 = coalesce(g1)
      (g2, newRemoved) match {
        case (graph, removedNodes) =>
          if (graph.nodes.forall(graph.precoloured)) {
            Eval.now((select(graph, removedNodes).colours.flatMap {
              case (k, v) => k.map(_ -> R(v))
            }, fun))
          } else if (graph.nodes.exists(n => graph.degree(n) < colourCount && !graph.moveRelated(n) && !graph.precoloured(n))) {
            simplifyCoalesce(spilled, fun, graph, removedNodes)
          } else if (graph.preferenceEdges.nonEmpty) {
            removeConflicting(spilled, fun, graph, removedNodes)
          } else {
            spill(spilled, graph, graph.nodes)
          }
      }
    }

    def removeConflicting(spilled: Set[ir.Register], fun: ir.Def.Fun, graph: Graph, removedNodes: RemovedNodes): Eval[(Map[ir.Register, Alloc], ir.Def.Fun)] = {
      val conflicting = graph.preferenceEdges.filter {
        case (a, b) => graph.interfering(a, b)
      }
      if(conflicting.nonEmpty) {
        simplifyCoalesce(spilled, fun, graph.withoutPreferenceEdges(conflicting), removedNodes)
      } else {
        freeze(spilled, fun, graph, removedNodes)
      }
    }

    def freeze(spilled: Set[ir.Register], fun: ir.Def.Fun, graph: Graph, removedNodes: RemovedNodes): Eval[(Map[ir.Register, Alloc], ir.Def.Fun)] = {
      graph.preferenceEdges.find {
        case (a, b) => graph.degree(a) < colourCount || graph.degree(b) < colourCount
      } match {
        case None => spill(spilled, graph, graph.nodes)
        case Some(e) => simplifyCoalesce(spilled, fun, graph.withoutPreferenceEdges(Set(e)), removedNodes)
      }
    }

    def spill(spilled: Set[ir.Register], graph: Graph, candidates: Set[Set[ir.Register]]): Eval[(Map[ir.Register, Alloc], ir.Def.Fun)] = {
      val spillable = candidates.filter(!graph.colours.contains(_)).toVector.sortBy { r =>
        - graph.neighbors(r).size // Sort by most neighbours
      }
      val regToSpill = spillable.head.head
      val newSpilled = spilled + regToSpill
      val spillfill = rebuildAfterSpill(originalFun, newSpilled)
      val g = buildInterferenceGraph(spillfill) -- newSpilled
      val newGraph = g.copy(colours = g.colours ++ precoloured).mapVar(Set(_))
      Eval.defer(simplifyCoalesce(newSpilled, spillfill, newGraph, Nil)).map(_.leftMap(_.updated(regToSpill, Spill)))
    }

    val g = buildInterferenceGraph(originalFun)
    val initialGraph: Graph = g.copy(colours = g.colours ++ precoloured).mapVar(Set(_))
    simplifyCoalesce(Set.empty, originalFun, initialGraph, Nil).value
  }

}
