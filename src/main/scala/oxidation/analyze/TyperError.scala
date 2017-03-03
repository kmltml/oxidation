package oxidation
package analyze

sealed trait TyperError

object TyperError {

  final case class CantMatch(expected: ExpectedType, found: Type) extends TyperError
  final case class SymbolNotFound(symbol: Symbol) extends TyperError

}
