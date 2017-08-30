package oxidation
package codegen.pass

import codegen.Name
import ir._
import backend.shared.FlowGraph

import scala.annotation.tailrec

object DeadBlockRemoval extends IdPass {

  override val name = "dead-block-removal"

  override val onDef = {
    case d @ Def.Fun(_, _, _, body, _) =>
      Vector(d.copy(body = transformBlocks(body)))
    case d @ Def.ComputedVal(_, body, _, _) =>
      Vector(d.copy(body = transformBlocks(body)))
  }

  private[pass] def transformBlocks(blocks: Vector[Block]): Vector[Block] = {
    val graph = FlowGraph(blocks)
    @tailrec
    def collectReachable(toVisit: List[Name], visited: Set[Name]): Set[Name] = toVisit match {
      case Nil => visited
      case current :: rest if visited(current) => collectReachable(rest, visited)
      case current :: rest =>
        val neighbors = graph.children(current).filter(!visited.contains(_))
        collectReachable(neighbors ++: toVisit, visited + current)
    }
    val reachable = collectReachable(blocks.head.name :: Nil, Set.empty)
    blocks.filter(b => reachable(b.name))
  }

}
