package oxidation
package backend
package shared

import <->.EdgeSyntax

import cats._
import cats.data._
import cats.implicits._

case class InterferenceGraph[Var, Reg](nodes: Set[Var], colours: Map[Var, Reg],
                                       interferenceEdges: Set[Edge[Var]],
                                       preferenceEdges: Set[Edge[Var]]) {

  def interfering(a: Var, b: Var): Boolean =
    interferenceEdges.contains(a <-> b) || interferenceEdges.contains(b <-> a)

  def -(v: Var): InterferenceGraph[Var, Reg] =
    InterferenceGraph(nodes - v, colours - v, interferenceEdges.filter {
      case a <-> b => !(a == v || b == v)
    }, preferenceEdges.filter {
      case a <-> b => !(a == v || b == v)
    })

  def neighbors(v: Var): Set[Var] = interferenceEdges.collect {
    case `v` <-> a => a
    case a <-> `v` => a
  }

  def moveNeighbors(v: Var): Set[Var] = preferenceEdges.collect {
    case `v` <-> a => a
    case a <-> `v` => a
  }

  def degree(node: Var): Int = interferenceEdges.count {
    case a <-> b => a == node || b == node
  }

  def mapVar[A](f: Var => A): InterferenceGraph[A, Reg] =
    InterferenceGraph(nodes.map(f), colours.map { case (k, v) => f(k) -> v },
      interferenceEdges.map(_.map(f)), preferenceEdges.map(_.map(f)))

  def moveRelated(v: Var): Boolean = preferenceEdges.exists {
    case a <-> b => a == v || b == v
  }

  def precoloured(v: Var): Boolean = colours.contains(v)

}

object InterferenceGraph {

  implicit def interferenceGraphMonoid[Var, Reg]: Monoid[InterferenceGraph[Var, Reg]] = new Monoid[InterferenceGraph[Var, Reg]] {

    override def empty: InterferenceGraph[Var, Reg] = InterferenceGraph(Set.empty, Map.empty, Set.empty, Set.empty)

    override def combine(x: InterferenceGraph[Var, Reg], y: InterferenceGraph[Var, Reg]): InterferenceGraph[Var, Reg] =
      InterferenceGraph(x.nodes |+| y.nodes, x.colours ++ y.colours,
        x.interferenceEdges |+| y.interferenceEdges, x.preferenceEdges |+| y.preferenceEdges)

  }

}

class Edge[A](val a: A, val b: A) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case x <-> y => (x == a && y == b) || (x == b && y == a)
    case _ => false
  }

  override def hashCode(): Int = a.## ^ b.##

  def swap: Edge[A] = b <-> a

  override def toString: String = s"$a <-> $b"

  def map[B](f: A => B): Edge[B] = f(a) <-> f(b)

}

object <-> {

  def apply[A](a: A, b: A): Edge[A] = new Edge(a, b)

  def unapply[A](edge: Edge[A]): Option[(A, A)] = Some(edge.a -> edge.b)

  implicit class EdgeSyntax[A](private val a: A) extends AnyVal {
    def <->(b: A): Edge[A] = new Edge(a, b)
  }

}
