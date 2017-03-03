package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

object SymbolResolver {

  type Scope = Symbols

  sealed trait Error extends AnalysisError
  final case class SymbolNotFound(name: String) extends Error
  final case class AmbiguousSymbolReference(name: String, candidates: Set[Symbol]) extends Error
  final case class SymbolNotFoundInImport(path: Seq[String]) extends Error

  def resolveSymbols(compilationUnit: Vector[parse.ast.TLD], global: Symbols): Either[Error, Vector[parse.ast.TLD]] = {
    val moduleImports = compilationUnit.collect {
      case parse.ast.Module(path) => path
    } .scanLeft(Vector.empty[String])(_ ++ _)
      .foldMap(prefix => global.findPrefixed(prefix))
    for {
      explicitImports <- compilationUnit.collect {
        case parse.ast.Import(path, parse.ast.ImportSpecifier.All) => Right(global.findPrefixed(path))
        case parse.ast.Import(path, parse.ast.ImportSpecifier.Members(members)) =>
          members.toVector.traverseU { n =>
            val syms = global.findExact(path :+ n)
            if(syms.isEmpty) Left(SymbolNotFoundInImport(path :+ n)) else Right(syms)
          }.map(_.reduce(_ |+| _))
      }.sequenceU
      imports = explicitImports.foldLeft(moduleImports)(_ |+| _)
      values <- compilationUnit.map {
        case m: parse.ast.Module => m.asRight
        case i: parse.ast.Import => i.asRight
        case d: parse.ast.Def => solveDef(d, imports)
      }.sequenceU
    } yield values
  }

  private def solveDef(d: parse.ast.Def, scope: Scope): Either[Error, parse.ast.Def] = d match {
    case parse.ast.ValDef(name, tpe, value) =>
      solveExpr(value, scope).map(parse.ast.ValDef(name, tpe, _))
    case parse.ast.VarDef(name, tpe, value) =>
      solveExpr(value, scope).map(parse.ast.VarDef(name, tpe, _))
    case parse.ast.DefDef(name, params, tpe, value) =>
      val newScope = params.getOrElse(Seq.empty)
        .foldLeft(scope)((s, p) => s.shadowTerm(Symbol.Local(p.name)))
      for {
        newParams <- params.traverseU(_.toVector.traverseU {
          case parse.ast.Param(name, tpe) => solveType(tpe, scope).map(parse.ast.Param(name, _))
        })
        newType <- tpe.traverseU(solveType(_, scope))
        newValue <- solveExpr(value, newScope)
      } yield parse.ast.DefDef(name, newParams, newType, newValue)
    case parse.ast.StructDef(name, params, members) =>
      val localScope = params.getOrElse(Seq.empty).foldLeft(scope)((s, l) => s.shadowType(Symbol.Local(l)))
      members.toVector.traverseU {
        case parse.ast.StructMember(name, tpe) => solveType(tpe, localScope).map(parse.ast.StructMember(name, _))
      }.map(parse.ast.StructDef(name, params, _))
  }

  private def solveExpr(e: parse.ast.Expression, scope: Scope): Either[Error, parse.ast.Expression] = e match {
    case parse.ast.Var(Symbol.Unresolved(n)) =>
      getOnlyOneSymbol(n, scope.terms).map(parse.ast.Var)
    case parse.ast.InfixAp(op, left, right) =>
      for {
        newLeft <- solveExpr(left, scope)
        newRight <- solveExpr(right, scope)
      } yield parse.ast.InfixAp(op, newLeft, newRight)

    case parse.ast.PrefixAp(op, exp) =>
      for {
        newExp <- solveExpr(exp, scope)
      } yield parse.ast.PrefixAp(op, newExp)
    case parse.ast.App(expr, params) =>
      for {
        newExpr <- solveExpr(expr, scope)
        newParams <- params.toVector.traverseU(solveExpr(_, scope))
      } yield parse.ast.App(newExpr, newParams)
    case parse.ast.Select(expr, member) =>
      for {
        newExpr <- solveExpr(expr, scope)
      } yield parse.ast.Select(newExpr, member)
    case parse.ast.If(cond, pos, neg) =>
      for {
        newCond <- solveExpr(cond, scope)
        newPos <- solveExpr(pos, scope)
        newNeg <- neg.traverseU(solveExpr(_, scope))
      } yield parse.ast.If(newCond, newPos, newNeg)
    case parse.ast.While(cond, body) =>
      for {
        newCond <- solveExpr(cond, scope)
        newBody <- solveExpr(body, scope)
      } yield parse.ast.While(newCond, newBody)
    case parse.ast.Assign(left, op, right) =>
      for {
        newLeft <- solveExpr(left, scope)
        newRight <- solveExpr(right, scope)
      } yield parse.ast.Assign(newLeft, op, newRight)

    case parse.ast.Block(stmnts) =>
      // TODO should forward references be reported here, or should a later pass take care of them?
      val locals = stmnts.collect {
        case parse.ast.ValDef(name, _, _) => name
        case parse.ast.VarDef(name, _, _) => name
      }
      val interiorScope = locals.foldLeft(scope)((s, l) => s.shadowTerm(Symbol.Local(l)))
      stmnts.toVector.traverseU {
        case d: parse.ast.Def => solveDef(d, interiorScope)
        case e: parse.ast.Expression => solveExpr(e, interiorScope)
      }.map(parse.ast.Block)

    case _: parse.ast.IntLit | _: parse.ast.BoolLit | _: parse.ast.StringLit => Right(e)
  }

  private def solveType(t: parse.ast.Type, scope: Scope): Either[Error, parse.ast.Type] = t match {
    case parse.ast.Type.Named(Symbol.Unresolved(sym)) =>
      getOnlyOneSymbol(sym, scope.types).map(parse.ast.Type.Named)
    case parse.ast.Type.App(const, params) =>
      for {
        newConst <- solveType(const, scope)
        newParams <- params.toVector.traverseU(solveType(_, scope))
      } yield parse.ast.Type.App(newConst, newParams)
  }

  private def getOnlyOneSymbol(s: String, scope: Multimap[String, Symbol]): Either[Error, Symbol] =
    scope.get(s)
      .toRight(SymbolNotFound(s))
      .flatMap {
        case ss if ss.size == 1 => Right(ss.head)
        case ss => Left(AmbiguousSymbolReference(s, ss))
      }
}
