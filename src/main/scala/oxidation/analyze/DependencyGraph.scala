package oxidation
package analyze

case class DependencyGraph(dependencies: Map[Symbol, DependencyEntry]) {

  def prune: DependencyGraph = {
    val deps = dependencies.mapValues {
      case DependencyEntry(deps, tpe) =>
        DependencyEntry(deps.filter(dependencies(_).declaredType.isEmpty), tpe)
    }
    DependencyGraph(deps)
  }

}

object DependencyGraph {

  def build(defs: Vector[parse.ast.TLD]): DependencyGraph = {
    val deps = defs.collect {
      case d: parse.ast.TermDef => d.name -> DependencyEntry.build(d)
    }.toMap
    DependencyGraph(deps)
  }

}

case class DependencyEntry(dependencies: Set[Symbol], declaredType: Option[TypeName])

object DependencyEntry {

  def build(d: parse.ast.TermDef): DependencyEntry =
    DependencyEntry(parse.ast.listTermSymbols(d).collect {
      case s: Symbol.Global if !BuiltinSymbols.intrinsics.contains(s) => s
    }.toSet, d match {
      case parse.ast.DefDef(_, _, t, _) => t
      case parse.ast.ValDef(_, t, _) => t
      case parse.ast.VarDef(_, t, _) => t
    })

}
