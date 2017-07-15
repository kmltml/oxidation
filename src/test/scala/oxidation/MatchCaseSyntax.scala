package oxidation

import oxidation.analyze.Typed

trait MatchCaseSyntax {

  implicit def tupleToParseMatchCase(t: (parse.ast.Pattern, parse.ast.Expression)): parse.ast.MatchCase =
    parse.ast.MatchCase(t._1, None, t._2)

  implicit def tupleToTypedMatchCase(t: (Typed[analyze.ast.Pattern], Typed[analyze.ast.Expression])): analyze.ast.MatchCase =
    analyze.ast.MatchCase(t._1, None, t._2)

}
