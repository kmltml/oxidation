package oxidation
package analyze

import Type._
import utest._
import parse.{Span, ast => untyped}

import cats._
import cats.data._
import cats.implicits._

import Validated.{valid, invalidNel}

object TypeTraverseTests extends TestSuite
  with SymbolSyntax
  with TypedSyntax {

  val loc = Span(None, 0, 0)

  val tests = apply {
    "solveTree" - {
      "a simple interdependent graph" - {
        TypeTraverse.solveTree(DependencyGraph(Map(
          g('a) -> DependencyEntry(Set(g('b)), explicitType = false),
          g('b) -> DependencyEntry(Set(), explicitType = true)
        )), Vector(
          untyped.DefDef(g('a), Some(List(untyped.Param("x", TypeName.Named(g('i32))))), None,
            untyped.InfixAp(InfixOp.Eq, untyped.Var(l('x), loc), untyped.Var(g('b), loc), loc)),
          untyped.ValDef(g('b), None, untyped.IntLit(32, loc))
        ), Ctxt.default) ==> valid(Set(
          ast.DefDef(g('a), Some(List(ast.Param("x", I32))), None,
            ast.InfixAp(InfixOp.Eq, ast.Var(l('x), loc) :: I32, ast.Var(g('b), loc) :: I32, loc) :: U1),
          ast.ValDef(g('b), None, ast.IntLit(32, loc) :: I32)
        ))
      }
      "type a def as a function type" - {
        TypeTraverse.solveTree(DependencyGraph(Map(
          g('a) -> DependencyEntry(Set(), explicitType = false),
          g('b) -> DependencyEntry(Set(g('a)), explicitType = false)
        )), Vector(
          untyped.DefDef(g('a), Some(List(untyped.Param("x", TypeName.Named(g('i32))))), None, untyped.Var(l('x), loc)),
          untyped.ValDef(g('b), None, untyped.Var(g('a), loc))
        ), Ctxt.default) ==> valid(Set(
          ast.DefDef(g('a), Some(List(ast.Param("x", I32))), None, ast.Var(l('x), loc) :: I32),
          ast.ValDef(g('b), None, ast.Var(g('a), loc) :: Fun(List(I32), I32))
        ))
      }
      "a recursive def with type annotation" - {
        TypeTraverse.solveTree(DependencyGraph(Map(
          g('factorial) -> DependencyEntry(Set(), explicitType = true)
        )), Vector(
          untyped.DefDef(g('factorial), Some(List(untyped.Param("i", TypeName.Named(g('i64))))), Some(TypeName.Named(g('i64))),
            untyped.App(untyped.Var(g('factorial), loc), List(untyped.Var(l('i), loc)), loc))
        ), Ctxt.default) ==> valid(Set(
          ast.DefDef(g('factorial), Some(List(ast.Param("i", I64))), Some(TypeName.Named(g('i64))),
            ast.App(ast.Var(g('factorial), loc) :: Fun(List(I64), I64), List(ast.Var(l('i), loc) :: I64), loc) :: I64)
        ))
      }
    }
  }

}
