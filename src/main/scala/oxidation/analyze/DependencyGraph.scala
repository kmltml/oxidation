package oxidation
package analyze

case class DependencyGraph(dependencies: Map[Symbol, DependencyEntry]) {

  def prune: DependencyGraph = {
    val deps = dependencies.mapValues {
      case DependencyEntry(deps, tpe) =>
        DependencyEntry(deps.filter(!dependencies(_).explicitType), tpe)
    }
    DependencyGraph(deps)
  }

}

object DependencyGraph {

  def build(defs: Vector[parse.ast.TLD]): DependencyGraph = {
    val deps = defs.collect {
      case d: parse.ast.TermDef => List(d.name -> DependencyEntry.build(d))
      case parse.ast.EnumDef(_, _, variants) =>
        variants.map {
          case EnumVariantDef(name, _) => name -> DependencyEntry(Set.empty, true)
        }
    }.flatten.toMap
    DependencyGraph(deps)
  }

}

case class DependencyEntry(dependencies: Set[Symbol], explicitType: Boolean)

object DependencyEntry {

  def build(d: parse.ast.TermDef): DependencyEntry =
    DependencyEntry(parse.ast.listTermSymbols(d).collect {
      case s: Symbol.Global if !BuiltinSymbols.intrinsics.contains(s) => s
    }.toSet, d match {
      case parse.ast.DefDef(_, _, t, _) => t.nonEmpty
      case parse.ast.ValDef(_, t, _) => t.nonEmpty
      case parse.ast.VarDef(_, t, _) => t.nonEmpty
    })

}
