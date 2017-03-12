package oxidation
package analyze

import Type._
import utest._
import parse.{ast => untyped}

object TypeTraverseTests extends TestSuite
  with SymbolSyntax
  with TypedSyntax {

  val tests = apply {
    "solveTree" - {
      "a simple interdependent graph" - {
        TypeTraverse.solveTree(DependencyGraph(Map(
          g('a) -> DependencyEntry(Set(g('b)), None),
          g('b) -> DependencyEntry(Set(), Some(TypeName.Named(g('i64))))
        )), Vector(
          untyped.DefDef(g('a), Some(Seq(Param("x", TypeName.Named(g('i32))))), None,
            untyped.InfixAp(InfixOp.Eq, untyped.Var(l('x)), untyped.Var(g('b)))),
          untyped.ValDef(g('b), None, untyped.IntLit(32))
        ), Ctxt.default) ==> Right(Set(
          ast.DefDef(g('a), Some(Seq(Param("x", TypeName.Named(g('i32))))), None,
            ast.InfixAp(InfixOp.Eq, ast.Var(l('x)) :: I32, ast.Var(g('b)) :: I32) :: U1),
          ast.ValDef(g('b), None, ast.IntLit(32) :: I32)
        ))
      }
      "type a def as a function type" - {
        TypeTraverse.solveTree(DependencyGraph(Map(
          g('a) -> DependencyEntry(Set(), None),
          g('b) -> DependencyEntry(Set(g('a)), None)
        )), Vector(
          untyped.DefDef(g('a), Some(Seq(Param("x", TypeName.Named(g('i32))))), None, untyped.Var(l('x))),
          untyped.ValDef(g('b), None, untyped.Var(g('a)))
        ), Ctxt.default) ==> Right(Set(
          ast.DefDef(g('a), Some(Seq(Param("x", TypeName.Named(g('i32))))), None, ast.Var(l('x)) :: I32),
          ast.ValDef(g('b), None, ast.Var(g('a)) :: Fun(Seq(I32), I32))
        ))
      }
    }
  }

}
