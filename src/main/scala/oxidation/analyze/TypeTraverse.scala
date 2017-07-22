package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._
import parse.{ast => untyped}

object TypeTraverse {

  case class CyclicReferenceError(cycle: Set[Symbol]) extends AnalysisError

  case class SolutionContext(types: Ctxt, solved: Map[Symbol, ast.TermDef])

  private type S[A] = State[Ctxt, A]
  private val S = MonadState[S, Ctxt]

  def solveTree(deps: DependencyGraph, defs: Vector[untyped.Def], ctxt: Ctxt): ValidatedNel[AnalysisError, Set[ast.Def]] = {

    val roots = deps.dependencies.values.foldLeft(deps.dependencies.keySet)((roots, s) => roots -- s.dependencies).toVector // TODO some cycles may not be found

    def postOrder(from: Symbol, processed: Set[Symbol]): Either[AnalysisError, Vector[Symbol]] = {
      for {
        _ <- Either.cond(!processed(from), (), CyclicReferenceError(processed))
        children <- deps.dependencies(from).dependencies.toVector.traverse(postOrder(_, processed + from))
      } yield children.flatten :+ from
    }
    val defsByName = defs.collect {
      case d: untyped.TermDef => d.name -> d
    }.toMap
    val orderedDefs = roots.traverse(postOrder(_, Set.empty)).map(_.flatten.distinct.flatMap(defsByName.get))
    assert(orderedDefs.forall(_.toSet === defsByName.values.toSet))

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
    orderedDefs.toValidatedNel.andThen { od =>
      od.collect {
        case d: untyped.TermDef => d
      }.traverse(solveTermDef(deps, Set.empty, defsByName, _))
        .runA(ctxt.withTerms(explicitDefs)).value.sequence.map(_.toSet)
    }
  }

  private def solveTermDef(deps: DependencyGraph, currentlySolved: Set[Symbol],
                           defs: Map[Symbol, untyped.TermDef], d: untyped.TermDef): S[ValidatedNel[AnalysisError, ast.TermDef]] = {
    for {
      ctxt <- S.get
      typed = Typer.solveTermDef(d, ctxt).leftMap(_.widen[AnalysisError])
      _ <- typed.toOption.traverse(solved)
    } yield typed
  }

  private def solved(d: ast.TermDef): S[Unit] =
    S.modify { ctxt =>
      d match {
        case ast.DefDef(name, Some(params), _, body) =>
          val paramTypes = params.map(_.typ)
          ctxt.withTerms(Map(name -> Ctxt.Immutable(Type.Fun(paramTypes, body.typ))))
        case _: ast.ValDef | ast.DefDef(_, None, _, _) =>
          ctxt.withTerms(Map(d.name -> Ctxt.Immutable(d.body.typ)))
        case _: ast.VarDef =>
          ctxt.withTerms(Map(d.name -> Ctxt.Mutable(d.body.typ)))
      }
    }

}
