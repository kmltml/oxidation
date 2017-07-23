package oxidation

trait SymbolSyntax {

  def u(s: scala.Symbol*): Symbol = Symbol.Unresolved(s.map(_.name).toList)
  def l(s: scala.Symbol): Symbol = Symbol.Local(s.name)
  def g(s: scala.Symbol*): Symbol = Symbol.Global(s.map(_.name).toList)

}
