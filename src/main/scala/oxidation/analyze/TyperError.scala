package oxidation
package analyze

import oxidation.parse.Span

sealed trait TyperError extends AnalysisError

object TyperError {

  final case class CantMatch(expected: ExpectedType, found: Type, loc: Span) extends TyperError
  final case class SymbolNotFound(symbol: Symbol, loc: Span) extends TyperError
  final case class WrongNumberOfArguments(expected: Int, found: Int, loc: Span) extends TyperError
  final case class MemberNotFound(name: String, in: Type, loc: Span) extends TyperError
  final case class NotAStruct(typ: Type, loc: Span) extends TyperError
  final case class WrongStructMembers(expected: Set[String], found: Set[String], loc: Span) extends TyperError
  final case class ImmutableAssign(symbol: Symbol, loc: Span) extends TyperError
  final case class NotAnLVal(expr: Typed[ast.Expression], loc: Span) extends TyperError
  final case class TooManyParamsForPointerDereference(found: Int, loc: Span) extends TyperError
  final case class ExternNoExplicitType(loc: Span) extends TyperError
  final case class NotASingletonType(found: TypeName) extends TyperError
  final case class NonexhaustivePatternMatch(unhandled: MatchSet, loc: Span) extends TyperError
  final case class AlternativePatternBindingsMismatch(leftBindings: Set[(Symbol, Ctxt.Term)], rightBindings: Set[(Symbol, Ctxt.Term)], loc: Span) extends TyperError

}
