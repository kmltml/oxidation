package oxidation
package analyze

import parse.{ast => P}
import Type._
import oxidation.analyze.Typer.solveType
import utest._

import cats._
import cats.data._
import cats.implicits._

object TyperTests extends TestSuite with SymbolSyntax with TypedSyntax {

  def findType(expr: P.Expression, expectedType: ExpectedType, ctxt: Ctxt = Ctxt.default): Either[TyperError, Type] =
    solveType(expr, expectedType, ctxt).map(_.typ)

  val tests = apply {
    "solveType" - {
      "int literals" - {
        findType(P.IntLit(20), ExpectedType.Undefined, Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Numeric, Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Specific(U32), Ctxt.empty) ==> Right(U32)
      }
      "bool literals" - {
        findType(P.BoolLit(true), ExpectedType.Undefined) ==> Right(U1)
        findType(P.BoolLit(false), ExpectedType.Specific(U1)) ==> Right(U1)
      }
      "operator expressions" - {
        findType(P.InfixAp(InfixOp.Add, P.IntLit(5), P.IntLit(10)), ExpectedType.Undefined, Ctxt.empty) ==>
          Right(I32)
        findType(P.InfixAp(InfixOp.Mul, P.Var(l('x)), P.Var(l('y))), ExpectedType.Numeric,
          Ctxt.terms(l('x) -> U8, l('y) -> U32)) ==> Right(U32)

        findType(P.InfixAp(InfixOp.Eq, P.IntLit(1), P.IntLit(2)), ExpectedType.Undefined, Ctxt.empty) ==> Right(U1)
      }
      "unary prefix operator expressions" - {
        findType(P.PrefixAp(PrefixOp.Inv, P.IntLit(64)), ExpectedType.Numeric) ==> Right(I32)
        findType(P.PrefixAp(PrefixOp.Neg, P.Var(l('x))), ExpectedType.Undefined, Ctxt.terms(l('x) -> U64)) ==> Right(I64)
        findType(P.PrefixAp(PrefixOp.Not, P.BoolLit(false)), ExpectedType.Undefined) ==> Right(U1)
      }
      "block expression" - {
        findType(P.Block(Seq(
          P.Var(l('x)), P.Var(l('y))
        )), ExpectedType.Undefined, Ctxt.terms(l('x) -> U8, l('y) -> U16)) ==> Right(U16)

        solveType(P.Block(Seq(
          P.ValDef(l('x), None, P.IntLit(10)),
          P.InfixAp(InfixOp.Add, P.Var(l('x)), P.IntLit(1))
        )), ExpectedType.Undefined, Ctxt.empty) ==> Right(ast.Block(Seq(
          ast.ValDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.IntLit(1) :: I32) :: I32
        )) :: I32)

        findType(P.Block(Seq(
          P.ValDef(l('x), Some(TypeName.Named(g('i64))), P.IntLit(10)),
          P.Var(l('x))
        )), ExpectedType.Numeric) ==> Right(I64)
      }
      findType(P.App(P.Var(g('foo)), Seq(P.IntLit(32))), ExpectedType.Undefined,
        Ctxt.terms(g('foo) -> Fun(Seq(I64), U1))) ==> Right(U1)
    }
  }

}
