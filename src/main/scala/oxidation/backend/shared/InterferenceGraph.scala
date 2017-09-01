package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._

case class InterferenceGraph[Var, Reg] private (colours: Map[Var, Reg],
                                                interferenceNeighbors: Map[Var, Set[Var]],
                                                preferenceNeighbors: Map[Var, Set[Var]]) {

  private type Self = InterferenceGraph[Var, Reg]

  interferenceNeighbors.flatMap{ case (k, v) => v.map(k -> _) }.find {
    case (k, v) => (colours.get(k), colours.get(v)).map2(_ == _) getOrElse false
  } match {
    case None =>
    case Some((a, b)) => throw new AssertionError(s"Malcoloured graph! Interfering nodes $a and $b are both coloured with ${colours(a)}")
  }

  assert((interferenceNeighbors.values.flatten.toSet ++ preferenceNeighbors.values.flatten.toSet) subsetOf nodes)

  lazy val nodes: Set[Var] = colours.keySet ++ interferenceNeighbors.keySet ++ preferenceNeighbors.keySet

  def interferenceEdges: Set[(Var, Var)] = interferenceNeighbors.toSet[(Var, Set[Var])].flatMap {
    case (k, v) => v.map(k -> _)
  }

  def preferenceEdges: Set[(Var, Var)] = preferenceNeighbors.toSet[(Var, Set[Var])].flatMap {
    case (k, v) => v.map(k -> _)
  }

  def interfering(a: Var, b: Var): Boolean =
    neighbors(a) contains b

  def -(v: Var): Self =
    InterferenceGraph(colours - v,
      (interferenceNeighbors - v).mapValues(_ - v),
      (preferenceNeighbors - v).mapValues(_ - v))

  def --(vs: Set[Var]): Self = vs.foldLeft(this)(_ - _)

  def neighbors(v: Var): Set[Var] = interferenceNeighbors.get(v).orEmpty

  def moveNeighbors(v: Var): Set[Var] = preferenceNeighbors.get(v).orEmpty

  def degree(node: Var): Int = neighbors(node).groupBy(colours.get).toVector.foldMap {
    case (Some(_), _) => 1
    case (None, l) => l.size
  }

  def mapVar[A](f: Var => A): InterferenceGraph[A, Reg] =
    InterferenceGraph(
      colours.map { case (k, v) => f(k) -> v },
      interferenceNeighbors.map { case (k, v) => f(k) -> v.map(f) },
      preferenceNeighbors.map { case (k, v) => f(k) -> v.map(f) })

  def moveRelated(v: Var): Boolean = moveNeighbors(v).nonEmpty

  def precoloured(v: Var): Boolean = colours.contains(v)

  def withInterferenceEdge(a: Var, b: Var): Self =
    copy(interferenceNeighbors = interferenceNeighbors |+| Map(a -> Set(b), b -> Set(a)))

  def withInterferenceEdges(edges: Set[(Var, Var)]): Self = {
    val map = edges.flatMap(x => Set(x, x.swap)).groupBy(_._1).mapValues(_.map(_._2).toSet)
    copy(interferenceNeighbors = interferenceNeighbors |+| map)
  }

  def withoutInterferenceEdges(edges: Set[(Var, Var)]): Self = {
    val n = interferenceNeighbors.map {
      case (k, v) =>
        val toRemove = edges.collect {
          case (`k`, a) => a
          case (a, `k`) => a
        }
        k -> (v diff toRemove)
    }
    copy(interferenceNeighbors = n)
  }

  def withPreferenceEdge(a: Var, b: Var): Self =
    copy(preferenceNeighbors = preferenceNeighbors |+| Map(a -> Set(b), b -> Set(a)))

  def withPreferenceEdges(edges: Set[(Var, Var)]): Self = {
    val map = edges.flatMap(x => Set(x, x.swap)).map { case (k, v) => k -> Set(v) }.toMap
    copy(preferenceNeighbors = preferenceNeighbors |+| map)
  }

  def withoutPreferenceEdges(edges: Set[(Var, Var)]): Self = {
    val n = preferenceNeighbors.map {
      case (k, v) =>
        val toRemove = edges.collect {
          case (`k`, a) => a
          case (a, `k`) => a
        }
        k -> (v diff toRemove)
    }
    copy(preferenceNeighbors = n)
  }

  def withColours(c: Map[Var, Reg]): Self =
    copy(colours = colours ++ c)

  def withNodes(n: Set[Var]): Self =
    copy(interferenceNeighbors = interferenceNeighbors |+| n.map(_ -> Set.empty[Var]).toMap,
         preferenceNeighbors = preferenceNeighbors |+| n.map(_ -> Set.empty[Var]).toMap)

}

object InterferenceGraph {

  def empty[Var, Reg]: InterferenceGraph[Var, Reg] = InterferenceGraph(Map.empty, Map.empty, Map.empty)

  implicit def interferenceGraphMonoid[Var, Reg]: Monoid[InterferenceGraph[Var, Reg]] = new Monoid[InterferenceGraph[Var, Reg]] {

    override def empty: InterferenceGraph[Var, Reg] = InterferenceGraph.empty

    override def combine(x: InterferenceGraph[Var, Reg], y: InterferenceGraph[Var, Reg]): InterferenceGraph[Var, Reg] =
      InterferenceGraph(x.colours ++ y.colours,
        x.interferenceNeighbors |+| y.interferenceNeighbors,
        x.preferenceNeighbors |+| y.preferenceNeighbors) ensuring (_.interferenceEdges == (x.interferenceEdges |+| y.interferenceEdges))

  }

}
