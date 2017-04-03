package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

object SymbolResolver {

  type Scope = Symbols

  type Res[A] = Either[Error, A]

  sealed trait Error extends AnalysisError
  final case class SymbolNotFound(name: String) extends Error
  final case class AmbiguousSymbolReference(name: String, candidates: Set[Symbol]) extends Error
  final case class SymbolNotFoundInImport(path: List[String]) extends Error

  sealed trait DefContext
  final case class TopLevel(module: List[String]) extends DefContext
  case object Local extends DefContext

  def resolveSymbols(compilationUnit: Vector[parse.ast.TLD], global: Symbols): Res[Vector[parse.ast.TLD]] = {
    val modulePaths = compilationUnit.collect {
      case parse.ast.Module(path) => path
    }
    val moduleImports = modulePaths
      .scanLeft(List.empty[String])(_ ++ _)
      .foldMap(prefix => global.findPrefixed(prefix))
    val modulePath = modulePaths.toList.flatten
    for {
      explicitImports <- compilationUnit.collect {
        case parse.ast.Import(path, ImportSpecifier.All) => Right(global.findPrefixed(path))
        case parse.ast.Import(path, ImportSpecifier.Members(members)) =>
          members.toVector.traverse { n =>
            val syms = global.findExact(path :+ n)
            if(syms.isEmpty) Left(SymbolNotFoundInImport(path :+ n)) else Right(syms)
          }.map(_.reduce(_ |+| _))
      }.sequence
      imports = explicitImports.foldLeft(moduleImports)(_ |+| _)
      values <- compilationUnit.map {
        case m: parse.ast.Module => m.asRight
        case i: parse.ast.Import => i.asRight
        case d: parse.ast.Def => solveDef(d, imports, TopLevel(modulePath))
      }.sequence
    } yield values
  }

  private def solveDef(d: parse.ast.Def, scope: Scope, ctxt: DefContext): Res[parse.ast.Def] = d match {
    case parse.ast.ValDef(name, tpe, value) =>
      (tpe.traverse(solveType(_, scope)), solveExpr(value, scope))
        .map2(parse.ast.ValDef(solveDefName(name, ctxt), _, _))

    case parse.ast.VarDef(name, tpe, value) =>
      (tpe.traverse(solveType(_, scope)), solveExpr(value, scope))
        .map2(parse.ast.VarDef(solveDefName(name, ctxt), _, _))

    case parse.ast.DefDef(name, params, tpe, value) =>
//      def f[F[_], G](x: F[G]): F[G] = x
//      val x = f(solveExpr(value, scope): Either[Error, parse.ast.Expression])
      val newScope = params.getOrElse(Nil)
        .foldLeft(scope)((s, p) => s.shadowTerm(Symbol.Local(p.name)))
      (params.traverse(_.traverse {
        case parse.ast.Param(name, tpe) => solveType(tpe, scope).map(parse.ast.Param(name, _))
      }), tpe.traverse(solveType(_, scope)), solveExpr(value, newScope))
        .map3(parse.ast.DefDef(solveDefName(name, ctxt), _, _, _))

    case parse.ast.StructDef(name, params, members) =>
      val localScope = params.getOrElse(Nil).foldLeft(scope)((s, l) => s.shadowType(Symbol.Local(l)))
      members.traverse {
        case StructMemberDef(name, tpe) => solveType(tpe, localScope).map(StructMemberDef(name, _))
      }.map(parse.ast.StructDef(solveDefName(name, ctxt), params, _))

    case parse.ast.TypeAliasDef(name, params, body) =>
      val localScope = params.getOrElse(Nil).foldLeft(scope)((s, l) => s.shadowType(Symbol.Local(l)))
      solveType(body, localScope).map(parse.ast.TypeAliasDef(solveDefName(name, ctxt), params, _))
  }

  private def solveDefName(name: Symbol, ctxt: DefContext): Symbol = (name, ctxt) match {
    case (Symbol.Unresolved(n), TopLevel(module)) => Symbol.Global(module :+ n)
    case (Symbol.Unresolved(n), Local) => Symbol.Local(n)
    case (s, _) => s
  }

  private def solveExpr(e: parse.ast.Expression, scope: Scope): Res[parse.ast.Expression] = e match {
    case parse.ast.Var(Symbol.Unresolved(n)) =>
      getOnlyOneSymbol(n, scope.terms).map(parse.ast.Var)

    case parse.ast.StructLit(Symbol.Unresolved(name), members) =>
      (getOnlyOneSymbol(name, scope.types), members.traverse {
        case (n, e) => solveExpr(e, scope).map(n -> _)
      }).map2(parse.ast.StructLit(_, _))

    case parse.ast.InfixAp(op, left, right) =>
      (solveExpr(left, scope), solveExpr(right, scope))
        .map2(parse.ast.InfixAp(op, _, _))

    case parse.ast.PrefixAp(op, exp) =>
      SymbolResolver.solveExpr(exp, scope)
        .map(parse.ast.PrefixAp.apply(op, _))

    case parse.ast.App(expr, params) =>
      (solveExpr(expr, scope), params.traverse(solveExpr(_, scope)))
        .map2(parse.ast.App(_, _))

    case parse.ast.Select(expr, member) =>
      solveExpr(expr, scope).map(parse.ast.Select(_, member))

    case parse.ast.If(cond, pos, neg) =>
      (solveExpr(cond, scope), solveExpr(pos, scope), neg.traverse(solveExpr(_, scope)))
        .map3(parse.ast.If)

    case parse.ast.While(cond, body) =>
      (solveExpr(cond, scope), solveExpr(body, scope))
        .map2(parse.ast.While)

    case parse.ast.Assign(left, op, right) =>
      (solveExpr(left, scope), solveExpr(right, scope))
        .map2(parse.ast.Assign(_, op, _))

    case parse.ast.Block(stmnts) =>
      // TODO should forward references be reported here, or should a later pass take care of them?
      val locals = stmnts.collect {
        case parse.ast.ValDef(Symbol.Unresolved(name), _, _) => name
        case parse.ast.VarDef(Symbol.Unresolved(name), _, _) => name
      }
      val interiorScope = locals.foldLeft(scope)((s, l) => s.shadowTerm(Symbol.Local(l)))
      stmnts.toVector.traverse {
        case d: parse.ast.Def => solveDef(d, interiorScope, Local)
        case e: parse.ast.Expression => solveExpr(e, interiorScope)
      }.map(parse.ast.Block)

    case _: parse.ast.IntLit | _: parse.ast.BoolLit | _: parse.ast.StringLit | _: parse.ast.CharLit |
         _: parse.ast.Extern | _: parse.ast.UnitLit => Right(e)

    case parse.ast.Widen(_) | parse.ast.Ignore(_) => ??? // theese should only be present after the typer
  }

  private def solveType(t: TypeName, scope: Scope): Res[TypeName] = t match {
    case TypeName.Named(Symbol.Unresolved(sym)) =>
      getOnlyOneSymbol(sym, scope.types).map(TypeName.Named)

    case TypeName.App(const, params) =>
      (solveType(const, scope), params.traverse(solveType(_, scope)))
        .map2(TypeName.App)

  }

  private def getOnlyOneSymbol(s: String, scope: Multimap[String, Symbol]): Res[Symbol] =
    scope.get(s)
      .toRight(SymbolNotFound(s))
      .flatMap {
        case ss if ss.size == 1 => Right(ss.head)
        case ss => Left(AmbiguousSymbolReference(s, ss))
      }
}
