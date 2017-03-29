package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._
import <->.EdgeSyntax
import oxidation.ir.RegisterNamespace

class RegisterAllocator[Reg](val calleeSavedRegs: List[Reg], val callerSavedRegs: List[Reg]) {

  sealed trait Alloc
  final case class R(r: Reg) extends Alloc
  case object Spill extends Alloc

  sealed trait Event extends Product with Serializable
    { def location: Int }
  final case class Start(location: Int, register: ir.Register) extends Event
  final case class End(location: Int, register: ir.Register) extends Event
  final case class Call(location: Int) extends Event

  implicit val eventOrder: Ordering[Event] = {
    case (Start(x, _), End(y, _)) if x == y => 1
    case (Start(x, _), Call(y)) if x == y => 1
    case (End(x, _), Start(y, _)) if x == y => -1
    case (Call(x), Start(y, _)) if x == y => -1
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
    val regs = block.instructions.flatMap(_.regs)
    val lifetimes = regs.map(r => r -> RegisterLifetime.lifetime(r, instrs, inputs, outputs)).toMap
    val calls = instrs.collect {
      case (ir.Inst.Eval(_, ir.Op.Call(_, _)), i) => Call(i)
    }
    val sorted = (lifetimes.flatMap {
      case (r, (Some(s), Some(e))) => Vector(Start(s, r), End(e, r))
      case (r, (Some(s), None)) => Vector(Start(s, r))
      case (r, (None, Some(e))) => Vector(End(e, r))
      case _ => Vector.empty
    }.toVector ++ calls).sorted
    type F[A] = WriterT[State[Set[ir.Register], ?], Set[Edge[ir.Register]], A]
    val W = MonadWriter[F, Set[Edge[ir.Register]]]
    val S = new MonadState[F, Set[ir.Register]] {
      override def get: F[Set[ir.Register]] = WriterT.lift(State.get)
      override def set(s: Set[ir.Register]): F[Unit] = WriterT.lift(State.set(s))
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
      case Call(_) => for {
        live <- S.get
        _ <- W.tell(for (a <- live; b <- virtualRegs.keySet) yield a <-> b)
      } yield ()
    }
    val edges = f.written.runA(inputs).value
    val moves = instrs.collect {
      case (ir.Inst.Move(dest, ir.Op.Copy(ir.Val.R(src))), _) => dest <-> src
      case (ir.Inst.Move(dest, ir.Op.Widen(ir.Val.R(src))), _) => dest <-> src // seems reasonable, but i'm not sure, should be tested further
    }
    val callVRegs = if(calls.isEmpty) Set() else virtualRegs.keySet
    InterferenceGraph(regs.toSet ++ callVRegs, Map.empty, edges, moves.toSet)
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
    val interferenceGraph = fun.body.foldMap { b =>
      buildInterferenceSubGraph(b, inputs(b.name) ++ ghosts(b.name), outputs(b.name) ++ ghosts(b.name))
    }
    interferenceGraph.copy(colours = interferenceGraph.colours ++ virtualRegs.filterKeys(interferenceGraph.nodes))
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
      case a <-> b =>
        !graph.interfering(a, b) &&
        (graph.neighbors(a) ++ graph.neighbors(b)).count(graph.degree(_) >= colourCount) < colourCount &&
        !(graph.precoloured(a) && graph.precoloured(b)) &&
        !((graph.colours.get(a) orElse graph.colours.get(b)).exists(c => (graph.neighbors(a) ++ graph.neighbors(b)).flatMap(graph.colours.get).contains(c)))
        // TODO ^^ This is ugly, and not present in original paper, something should be done about this ^^
    } match {
      case None => graph
      case Some(a <-> b) =>
        val removed = graph - a - b
        val newNode = a ++ b
        coalesce(removed |+| InterferenceGraph(
          nodes = removed.nodes + newNode,
          colours = (graph.colours.get(a) orElse graph.colours.get(b)).map(r => Map(newNode -> r)) getOrElse Map.empty,
          interferenceEdges = (graph.neighbors(a) ++ graph.neighbors(b)).map(newNode <-> _),
          preferenceEdges = (graph.moveNeighbors(a) ++ graph.moveNeighbors(b) diff Set(a, b)).map(newNode <-> _)
        ))
    }
  }

  def select(graph: Graph, removed: RemovedNodes): Graph = removed match {
    case (node, neighbors) :: rest =>
      val neighborColors = neighbors
        .map(n => graph.nodes.find(n subsetOf _).get)
        .map(graph.colours(_))
      // Simplify should only remove nodes in such way, that there will be a color available when re-inserting it
      // This failing to find a register would signify a bug in the simplify method, not here.
      // Also, prefer caller saved regs, to minimize register saving in prologue
      val colour = (callerSavedRegs.find(!neighborColors(_)) orElse calleeSavedRegs.find(!neighborColors(_))).get
      select(graph |+| InterferenceGraph(
        nodes = graph.nodes + node,
        colours = graph.colours.updated(node, colour),
        interferenceEdges = neighbors.map(_ <-> node),
        preferenceEdges = Set.empty
      ), rest)
    case Nil => graph
  }

  def allocate(fun: ir.Def.Fun, precoloured: Map[ir.Register, Reg]): Map[ir.Register, Alloc] = {

    def simplifyCoalesce(initialGraph: Graph, graph: Graph, removedNodes: RemovedNodes): Map[ir.Register, Alloc] = {
      val (g1, newRemoved) = simplify(graph, removedNodes)
      val g2 = coalesce(g1)
      (g2, newRemoved) match {
        case (graph, removedNodes) =>
          if (graph.nodes.forall(graph.precoloured)) {
            select(graph, removedNodes).colours.flatMap {
              case (k, v) => k.map(_ -> R(v))
            }
          } else if (graph.nodes.exists(n => graph.degree(n) < colourCount && !graph.moveRelated(n) && !graph.precoloured(n))) {
            simplifyCoalesce(initialGraph, graph, removedNodes)
          } else if (graph.preferenceEdges.nonEmpty) {
            removeConflicting(initialGraph, graph, removedNodes)
          } else {
            spill(initialGraph, graph.nodes)
          }
      }
    }

    def removeConflicting(initialGraph: Graph, graph: Graph, removedNodes: RemovedNodes): Map[ir.Register, Alloc] = {
      val conflicting = graph.preferenceEdges.filter {
        case a <-> b => graph.interfering(a, b)
      }
      if(conflicting.nonEmpty) {
        simplifyCoalesce(initialGraph, graph.copy(preferenceEdges = graph.preferenceEdges -- conflicting), removedNodes)
      } else {
        freeze(initialGraph, graph, removedNodes)
      }
    }

    def freeze(initialGraph: Graph, graph: Graph, removedNodes: RemovedNodes): Map[ir.Register, Alloc] = {
      graph.preferenceEdges.find {
        case a <-> b => graph.degree(a) < colourCount || graph.degree(b) < colourCount
      } match {
        case None => spill(initialGraph, graph.nodes)
        case Some(e) => simplifyCoalesce(initialGraph, graph.copy(preferenceEdges = graph.preferenceEdges - e), removedNodes)
      }
    }

    def spill(initialGraph: Graph, candidates: Set[Set[ir.Register]]): Map[ir.Register, Alloc] = {
      val regToSpill = candidates.find(_.size == 1).map(_.head).getOrElse(candidates.flatten.head)
      val graph = initialGraph - Set(regToSpill)
      simplifyCoalesce(graph, graph, Nil).updated(regToSpill, Spill)
    }

    val g = buildInterferenceGraph(fun)
    val initialGraph: Graph = g.copy(colours = g.colours ++ precoloured).mapVar(Set(_))
    simplifyCoalesce(initialGraph, initialGraph, Nil)
  }

}
