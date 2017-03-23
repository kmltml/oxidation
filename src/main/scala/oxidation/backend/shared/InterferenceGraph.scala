package oxidation
package backend
package shared

import <->.EdgeSyntax

case class InterferenceGraph[Var, Reg](nodes: Set[Var], colours: Map[Var, Reg],
                                       interferenceEdges: Set[Edge[Var]]) {


  def interfering(a: Var, b: Var): Boolean =
    interferenceEdges.contains(a <-> b) || interferenceEdges.contains(b <-> a)

}

class Edge[A](val a: A, val b: A) {

  override def equals(obj: scala.Any): Boolean = obj match {
    case x <-> y => (x == a && y == b) || (x == b && y == a)
    case _ => false
  }

  override def hashCode(): Int = a.## ^ b.##

  def swap: Edge[A] = b <-> a

}

object <-> {

  def apply[A](a: A, b: A): Edge[A] = new Edge(a, b)

  def unapply[A](edge: Edge[A]): Option[(A, A)] = Some(edge.a -> edge.b)

  implicit class EdgeSyntax[A](private val a: A) extends AnyVal {
    def <->(b: A): Edge[A] = new Edge(a, b)
  }

}
