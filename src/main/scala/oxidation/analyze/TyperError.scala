package oxidation
package analyze

sealed trait TyperError extends AnalysisError

object TyperError {

  final case class CantMatch(expected: ExpectedType, found: Type) extends TyperError
  final case class SymbolNotFound(symbol: Symbol) extends TyperError
  final case class WrongNumberOfArguments(expected: Int, found: Int) extends TyperError

}
