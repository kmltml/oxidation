package oxidation
package backend
package shared

import codegen.Name
import codegen.ir

class FlowGraph(val blocks: Map[Name, ir.Block]) {

  lazy val parents: Memo[Name, Set[ir.Block]] = Memo { n =>
    val block = blocks(n)
    blocks.values.filter(b => children(b.name) contains block).toSet
  }

  lazy val children: Memo[Name, Set[ir.Block]] = Memo { n =>
    (blocks(n).flow match {
      case ir.FlowControl.Return(_) => Set.empty
      case ir.FlowControl.Branch(_, t, f) => Set(t, f)
      case ir.FlowControl.Goto(l) => Set(l)
    }).map(blocks)
  }

  def successors(name: Name): Set[ir.Block] = {
    def go(name: Name, found: Set[ir.Block]): Set[ir.Block] = {
      val search = children(name) -- found
      search.foldLeft(found)((f, b) => go(b.name, f))
    }
    go(name, Set.empty)
  }

}
