package oxidation
package analyze

import parse.{ast => P}
import Type._
import oxidation.analyze.Typer.solveType
import utest._

import cats._
import cats.data._
import cats.implicits._

object TyperTests extends TestSuite {

  def findType(expr: P.Expression, expectedType: ExpectedType, ctxt: Ctxt = Ctxt.empty): Either[TyperError, Type] =
    solveType(expr, expectedType, ctxt).map(_.typ)

  val tests = apply {
    "solveType" - {
      "int literals" - {
        findType(P.IntLit(20), ExpectedType.Undefined, Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Numeric, Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Specific(U32), Ctxt.empty) ==> Right(U32)
      }
      "operator expressions" - {
        findType(P.InfixAp(InfixOp.Add, P.IntLit(5), P.IntLit(10)), ExpectedType.Undefined, Ctxt.empty) ==>
          Right(I32)
        findType(P.InfixAp(InfixOp.Mul, P.Var("x"), P.Var("y")), ExpectedType.Numeric,
          Ctxt.terms("x" -> U8, "y" -> U32)) ==> Right(U32)

        findType(P.InfixAp(InfixOp.Eq, P.IntLit(1), P.IntLit(2)), ExpectedType.Undefined, Ctxt.empty) ==> Right(U1)
      }
      "block expression" - {
        findType(P.Block(Seq(
          P.Var("x"), P.Var("y")
        )), ExpectedType.Undefined, Ctxt.terms("x" -> U8, "y" -> U16)) ==> Right(U16)

        findType(P.Block(Seq(
          P.ValDef("x", None, P.IntLit(10)),
          P.InfixAp(InfixOp.Add, P.Var("x"), P.IntLit(1))
        )), ExpectedType.Undefined) ==> Right(I32)

        findType(P.Block(Seq(
          P.ValDef("x", Some(P.Type.Named("i64")), P.IntLit(10)),
          P.Var("x")
        )), ExpectedType.Numeric) ==> Right(I64)
      }
    }
  }

}
