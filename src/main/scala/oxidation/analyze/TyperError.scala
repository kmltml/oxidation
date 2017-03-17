package oxidation
package analyze

sealed trait TyperError extends AnalysisError

object TyperError {

  final case class CantMatch(expected: ExpectedType, found: Type) extends TyperError
  final case class SymbolNotFound(symbol: Symbol) extends TyperError
  final case class WrongNumberOfArguments(expected: Int, found: Int) extends TyperError
  final case class MemberNotFound(name: String, in: Type) extends TyperError
  final case class NotAStruct(typ: Type) extends TyperError
  final case class WrongStructMembers(expected: Set[String], found: Set[String]) extends TyperError
  final case class ImmutableAssign(symbol: Symbol) extends TyperError
  final case class NotAnLVal(expr: Typed[ast.Expression]) extends TyperError
  final case class TooManyParamsForPointerDereference(found: Int) extends TyperError

}
