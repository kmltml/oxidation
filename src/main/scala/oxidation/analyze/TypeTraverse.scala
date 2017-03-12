package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._
import parse.{ast => untyped}

object TypeTraverse {

  case class CyclicReferenceError(cycle: Set[Symbol]) extends AnalysisError

  case class SolutionContext(types: Ctxt, solved: Map[Symbol, ast.TermDef])

  private type S[A] = StateT[Either[AnalysisError, ?], SolutionContext, A]
  private val S = MonadState[S, SolutionContext]

  def solveTree(deps: DependencyGraph, defs: Vector[untyped.Def], ctxt: Ctxt): Either[AnalysisError, Set[ast.Def]] = {
    val defsByName = defs.collect {
      case d: untyped.TermDef => d.name -> d
    }.toMap
    defs.collect {
      case d: untyped.TermDef => d
    }.traverse(solveTermDef(deps, Set.empty, defsByName, _))
      .runS(SolutionContext(ctxt, Map.empty))
      .map(_.solved.values.toSet)
  }

  private def solveTermDef(deps: DependencyGraph, currentlySolved: Set[Symbol],
                           defs: Map[Symbol, untyped.TermDef], d: untyped.TermDef): S[Unit] = {
    for {
      ctxt <- S.get
      _ <- StateT.lift(Either.cond(!currentlySolved(d.name), (), CyclicReferenceError(currentlySolved): AnalysisError))
      _ <- if(ctxt.solved.contains(d.name)) S.pure(()) else for {
        _ <- deps.dependencies(d.name).dependencies.toVector
          .traverse(s => solveTermDef(deps, currentlySolved + d.name, defs, defs(s)))
        ctxt <- S.get
        typed <- StateT.lift(Typer.solveTermDef(d, ctxt.types).leftWiden[AnalysisError])
        _ <- solved(typed)
      } yield ()
    } yield ()
  }

  private def solved(d: ast.TermDef): S[Unit] =
    S.modify {
      case SolutionContext(types, solved) =>
        d match {
          case ast.DefDef(name, Some(params), _, body) =>
            val paramTypes = params.map(p => Typer.lookupType(p.typ, types))
            SolutionContext(
              types.withTerms(Map(name -> Type.Fun(paramTypes, body.typ))),
              solved.updated(name, d))
          case _: ast.ValDef | _: ast.VarDef | ast.DefDef(_, None, _, _) =>
            SolutionContext(
              types.withTerms(Map(d.name -> d.body.typ)),
              solved.updated(d.name, d))
        }
    }

}
