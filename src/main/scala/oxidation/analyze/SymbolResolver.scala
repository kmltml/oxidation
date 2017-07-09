package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._
import Validated.{invalidNel, valid}

object SymbolResolver {

  type Scope = Symbols

  type Res[A] = ValidatedNel[Error, A]

  sealed trait Error extends AnalysisError
  final case class SymbolNotFound(name: String) extends Error
  final case class AmbiguousSymbolReference(name: String, candidates: Set[Symbol]) extends Error
  final case class SymbolNotFoundInImport(path: List[String]) extends Error

  sealed trait DefContext
  final case class TopLevel(module: List[String]) extends DefContext
  case object Local extends DefContext

  def resolveSymbols(compilationUnit: Vector[parse.ast.TLD], global: Symbols): Either[NonEmptyList[Error], Vector[parse.ast.TLD]] = {
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
          members.traverse { n =>
            val syms = global.findExact(path :+ n)
            if(syms.isEmpty) Left(NonEmptyList.of(SymbolNotFoundInImport(path :+ n))) else Right(syms)
          }.map(_.reduce(_ |+| _))
      }.sequence
      imports = explicitImports.foldLeft(moduleImports)(_ |+| _)
      values <- compilationUnit.map {
        case m: parse.ast.Module => valid(m)
        case i: parse.ast.Import => valid(i)
        case d: parse.ast.Def => solveDef(d, imports, global, TopLevel(modulePath))
      }.sequence.toEither
    } yield values
  }

  private def solveDef(d: parse.ast.Def, scope: Scope, global: Symbols, ctxt: DefContext): Res[parse.ast.Def] = d match {
    case parse.ast.ValDef(name, tpe, value) =>
      (tpe.traverse(solveType(_, scope)), solveExpr(value, scope, global))
        .map2(parse.ast.ValDef(solveDefName(name, ctxt), _, _))

    case parse.ast.VarDef(name, tpe, value) =>
      (tpe.traverse(solveType(_, scope)), solveExpr(value, scope, global))
        .map2(parse.ast.VarDef(solveDefName(name, ctxt), _, _))

    case parse.ast.DefDef(name, params, tpe, value) =>
//      def f[F[_], G](x: F[G]): F[G] = x
//      val x = f(solveExpr(value, scope): Either[Error, parse.ast.Expression])
      val newScope = params.getOrElse(Nil)
        .foldLeft(scope)((s, p) => s.shadowTerm(Symbol.Local(p.name)))
      (params.traverse(_.traverse {
        case parse.ast.Param(name, tpe) => solveType(tpe, scope).map(parse.ast.Param(name, _))
      }), tpe.traverse(solveType(_, scope)), solveExpr(value, newScope, global))
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

  private def solveExpr(e: parse.ast.Expression, scope: Scope, global: Symbols): Res[parse.ast.Expression] = e match {
    case parse.ast.Var(Symbol.Unresolved(n), loc) =>
      getOnlyOneSymbol(n, scope.terms).map(parse.ast.Var(_, loc))

    case parse.ast.StructLit(Symbol.Unresolved(name), members, loc) =>
      (getOnlyOneSymbol(name, scope.types), members.traverse {
        case (n, e) => solveExpr(e, scope, global).map(n -> _)
      }).map2(parse.ast.StructLit(_, _, loc))

    case parse.ast.InfixAp(op, left, right, loc) =>
      (solveExpr(left, scope, global), solveExpr(right, scope, global))
        .map2(parse.ast.InfixAp(op, _, _, loc))

    case parse.ast.PrefixAp(op, exp, loc) =>
      SymbolResolver.solveExpr(exp, scope, global)
        .map(parse.ast.PrefixAp.apply(op, _, loc))

    case parse.ast.App(expr, params, loc) =>
      (solveExpr(expr, scope, global), params.traverse(solveExpr(_, scope, global)))
        .map2(parse.ast.App(_, _, loc))

    case parse.ast.TypeApp(expr, params, loc) =>
      (solveExpr(expr, scope, global), params.traverse(solveType(_, scope)))
        .map2(parse.ast.TypeApp(_, _, loc))

    case parse.ast.Select(expr, member, loc) =>
      val res = for {
        mod <- solveModule(expr, scope, global)
        s <- Some(Symbol.Global(mod :+ member)).filter(global.terms.values.flatten.toSeq.contains)
      } yield parse.ast.Var(s, loc)
      res.map(valid).getOrElse {
        solveExpr(expr, scope, global).map(parse.ast.Select(_, member, loc))
      }

    case parse.ast.If(cond, pos, neg, loc) =>
      (solveExpr(cond, scope, global), solveExpr(pos, scope, global), neg.traverse(solveExpr(_, scope, global)))
        .map3(parse.ast.If(_, _, _, loc))

    case parse.ast.While(cond, body, loc) =>
      (solveExpr(cond, scope, global), solveExpr(body, scope, global))
        .map2(parse.ast.While(_, _, loc))

    case parse.ast.Match(matchee, cases, loc) =>
      def solveCase(pattern: parse.ast.Pattern, body: parse.ast.Expression): Res[(parse.ast.Pattern, parse.ast.Expression)] = {
        import parse.ast.Pattern
        def findBindings(pattern: Pattern): List[Symbol] = pattern match {
            case Pattern.Var(Symbol.Unresolved(n), _) => List(Symbol.Local(n))
            case Pattern.Struct(_, members, _, _) => members.flatMap { case (_, p) => findBindings(p) }
            case Pattern.Ignore(_) | Pattern.IntLit(_, _) | Pattern.FloatLit(_, _) | Pattern.BoolLit(_, _)
               | Pattern.CharLit(_, _) => Nil
          }
        val bindings = findBindings(pattern)

        ( solvePattern(pattern, scope, global),
          solveExpr(body, bindings.foldLeft(scope)(_.shadowTerm(_)), global))
          .map2((_, _))
      }

      (solveExpr(matchee, scope, global), cases.traverse((solveCase _).tupled))
        .map2(parse.ast.Match(_, _, loc))

    case parse.ast.Assign(left, op, right, loc) =>
      (solveExpr(left, scope, global), solveExpr(right, scope, global))
        .map2(parse.ast.Assign(_, op, _, loc))

    case parse.ast.Block(stmnts, loc) =>
      // TODO should forward references be reported here, or should a later pass take care of them?
      val locals = stmnts.collect {
        case parse.ast.ValDef(Symbol.Unresolved(name), _, _) => name
        case parse.ast.VarDef(Symbol.Unresolved(name), _, _) => name
      }
      val interiorScope = locals.foldLeft(scope)((s, l) => s.shadowTerm(Symbol.Local(l)))
      stmnts.traverse {
        case d: parse.ast.Def => solveDef(d, interiorScope, global, Local)
        case e: parse.ast.Expression => solveExpr(e, interiorScope, global)
      }.map(parse.ast.Block(_, loc))

    case _: parse.ast.IntLit | _: parse.ast.BoolLit | _: parse.ast.StringLit | _: parse.ast.CharLit |
         _: parse.ast.Extern | _: parse.ast.UnitLit | _: parse.ast.FloatLit => valid(e)

    case parse.ast.Widen(_, _) | parse.ast.Ignore(_, _) => ??? // theese should only be present after the typer
  }

  private def solveType(t: TypeName, scope: Scope): Res[TypeName] = t match {
    case TypeName.Named(Symbol.Unresolved(sym)) =>
      getOnlyOneSymbol(sym, scope.types).map(TypeName.Named)

    case TypeName.App(const, params) =>
      (solveType(const, scope), params.traverse(solveType(_, scope)))
        .map2(TypeName.App)

    case l: TypeName.IntLiteral => valid(l)

  }

  private def solveModule(p: parse.ast.Expression, scope: Scope, global: Symbols): Option[List[String]] = p match {
    case parse.ast.Var(Symbol.Unresolved(v), _) =>
      scope.importedModules.get(v).filter(_.size == 1).map(_.head)
    case parse.ast.Select(p, m, _) =>
      for {
        prefix <- solveModule(p, scope, global)
        x <- Some(prefix :+ m).filter(global.modules.contains)
      } yield x
    case _ => None
  }

  def solvePattern(pattern: parse.ast.Pattern, scope: Scope, global: Symbols): Res[parse.ast.Pattern] = pattern match {
    case parse.ast.Pattern.Var(Symbol.Unresolved(n), loc) => valid(parse.ast.Pattern.Var(Symbol.Local(n), loc))
    case parse.ast.Pattern.Struct(typeName, members, ignoreExtra, loc) =>
      val solvedTypeName = typeName.traverse {
        case Symbol.Unresolved(s) => getOnlyOneSymbol(s, scope.types)
      }
      val solvedMembers = members.traverse {
        case (name, pat) => solvePattern(pat, scope, global).map(name -> _)
      }
      (solvedTypeName, solvedMembers)
        .map2(parse.ast.Pattern.Struct(_, _, ignoreExtra, loc))
    case parse.ast.Pattern.Ignore(_) | parse.ast.Pattern.IntLit(_, _) | parse.ast.Pattern.FloatLit(_, _)
         | parse.ast.Pattern.BoolLit(_, _) | parse.ast.Pattern.CharLit(_, _) => valid(pattern)
  }

  private def getOnlyOneSymbol(s: String, scope: Multimap[String, Symbol]): Res[Symbol] =
    scope.get(s)
      .toValidNel(SymbolNotFound(s))
      .andThen {
        case ss if ss.size == 1 => valid(ss.head)
        case ss => invalidNel(AmbiguousSymbolReference(s, ss))
      }
}
