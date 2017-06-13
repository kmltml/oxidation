package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._
import parse.{ast => untyped}

object TypeTraverse {

  case class CyclicReferenceError(cycle: Set[Symbol]) extends AnalysisError

  case class SolutionContext(types: Ctxt, solved: Map[Symbol, ast.TermDef])

  private type S[A] = StateT[Either[NonEmptyList[AnalysisError], ?], SolutionContext, A]
  private val S = MonadState[S, SolutionContext]

  def solveTree(deps: DependencyGraph, defs: Vector[untyped.Def], ctxt: Ctxt): Either[NonEmptyList[AnalysisError], Set[ast.Def]] = {
    val defsByName = defs.collect {
      case d: untyped.TermDef => d.name -> d
    }.toMap
    val explicitDefs = defs.collect {
      case untyped.DefDef(name, Some(params), Some(retName), _) =>
        val paramTypes = params.map(p => Typer.lookupType(p.typ, ctxt))
        val ret = Typer.lookupType(retName, ctxt)
        name -> Ctxt.Immutable(Type.Fun(paramTypes, ret))
      case (d: untyped.TermDef) if (d.typ.isDefined) =>
        val typ = Typer.lookupType(d.typ.get, ctxt)
        d.name -> (d match {
          case _: untyped.DefDef | _: untyped.ValDef => Ctxt.Immutable(typ)
          case _: untyped.VarDef => Ctxt.Mutable(typ)
        })
    }.toMap
    defs.collect {
      case d: untyped.TermDef => d
    }.traverse(solveTermDef(deps, Set.empty, defsByName, _))
      .runS(SolutionContext(ctxt.withTerms(explicitDefs), Map.empty))
      .map(_.solved.values.toSet)
  }

  private def solveTermDef(deps: DependencyGraph, currentlySolved: Set[Symbol],
                           defs: Map[Symbol, untyped.TermDef], d: untyped.TermDef): S[Unit] = {
    for {
      ctxt <- S.get
      _ <- StateT.lift(Either.cond(!currentlySolved(d.name), (), NonEmptyList.of(CyclicReferenceError(currentlySolved): AnalysisError)))
      _ <- if(ctxt.solved.contains(d.name)) S.pure(()) else for {
        _ <- deps.dependencies(d.name).dependencies.toVector
          .traverse(s => solveTermDef(deps, currentlySolved + d.name, defs, defs(s)))
        ctxt <- S.get
        typed <- StateT.lift(Typer.solveTermDef(d, ctxt.types).leftMap(_.widen[AnalysisError]).toEither)
        _ <- solved(typed)
      } yield ()
    } yield ()
  }

  private def solved(d: ast.TermDef): S[Unit] =
    S.modify {
      case SolutionContext(types, solved) =>
        d match {
          case ast.DefDef(name, Some(params), _, body) =>
            val paramTypes = params.map(_.typ)
            SolutionContext(
              types.withTerms(Map(name -> Ctxt.Immutable(Type.Fun(paramTypes, body.typ)))),
              solved.updated(name, d))
          case _: ast.ValDef | ast.DefDef(_, None, _, _) =>
            SolutionContext(
              types.withTerms(Map(d.name -> Ctxt.Immutable(d.body.typ))),
              solved.updated(d.name, d))
          case _: ast.VarDef =>
            SolutionContext(
              types.withTerms(Map(d.name -> Ctxt.Mutable(d.body.typ))),
              solved.updated(d.name, d))
        }
    }

}
