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

  def imm(t: Type): Ctxt.Term = Ctxt.Immutable(t)
  def mut(t: Type): Ctxt.Term = Ctxt.Mutable(t)

  val tests = apply {
    "solveType" - {
      "int literals" - {
        findType(P.IntLit(20), ExpectedType.Undefined, Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Numeric(None), Ctxt.empty) ==> Right(I32)
        findType(P.IntLit(20), ExpectedType.Specific(U32), Ctxt.empty) ==> Right(U32)
      }
      "bool literals" - {
        findType(P.BoolLit(true), ExpectedType.Undefined) ==> Right(U1)
        findType(P.BoolLit(false), ExpectedType.Specific(U1)) ==> Right(U1)
      }
      "CharLit" - {
        solveType(P.CharLit('a'), ExpectedType.Undefined, Ctxt.default) ==> Right(ast.CharLit('a') :: U8)
      }
      "struct literals" - {
        val Vec2 = Struct(g('Vec2), Seq(
          StructMember("x", I32), StructMember("y", I64)
        ))
        solveType(P.StructLit(g('Vec2), Seq(
          "x" -> P.IntLit(10), "y" -> P.IntLit(20)
        )), ExpectedType.Undefined, Ctxt.default.withTypes(Map(g('Vec2) -> Vec2))) ==>
          Right(ast.StructLit(g('Vec2), Seq(
            "x" -> (ast.IntLit(10) :: I32), "y" -> (ast.IntLit(20) :: I64)
          )) :: Vec2)
      }
      "operator expressions" - {
        findType(P.InfixAp(InfixOp.Add, P.IntLit(5), P.IntLit(10)), ExpectedType.Undefined, Ctxt.empty) ==>
          Right(I32)
        findType(P.InfixAp(InfixOp.Mul, P.Var(l('x)), P.Var(l('y))), ExpectedType.Numeric(None),
          Ctxt.terms(l('x) -> imm(U8), l('y) -> imm(U32))) ==> Right(U32)

        findType(P.InfixAp(InfixOp.Eq, P.IntLit(1), P.IntLit(2)), ExpectedType.Undefined, Ctxt.empty) ==> Right(U1)

        "widening in arithmetic expressions" - {
          "i32 + i8 -> i32" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x)), P.Var(l('y))), ExpectedType.Numeric(None),
              Ctxt.default.withTerms(Map(l('x) -> imm(I32), l('y) -> imm(I8)))) ==>
              Right(ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.Widen(ast.Var(l('y)) :: I8) :: I32) :: I32)
          }
          "i8 + i32 -> i32" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x)), P.Var(l('y))), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(I8), l('y) -> imm(I32)))) ==>
              Right(ast.InfixAp(InfixOp.Add, ast.Widen(ast.Var(l('x)) :: I8) :: I32, ast.Var(l('y)) :: I32) :: I32)
          }
          "(i8 + i32): i64 -> i64" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x)), P.Var(l('y))), ExpectedType.Specific(I64),
              Ctxt.default.withTerms(Map(l('x) -> imm(I8), l('y) -> imm(I32)))) ==>
              Right(ast.InfixAp(InfixOp.Add, ast.Widen(ast.Var(l('x)) :: I8) :: I64, ast.Widen(ast.Var(l('y)) :: I32) :: I64) :: I64)
          }
        }
      }
      "unary prefix operator expressions" - {
        findType(P.PrefixAp(PrefixOp.Inv, P.IntLit(64)), ExpectedType.Numeric(None)) ==> Right(I32)
        findType(P.PrefixAp(PrefixOp.Neg, P.Var(l('x))), ExpectedType.Undefined, Ctxt.terms(l('x) -> imm(U64))) ==> Right(I64)
        findType(P.PrefixAp(PrefixOp.Not, P.BoolLit(false)), ExpectedType.Undefined) ==> Right(U1)
      }
      "block expression" - {
        findType(P.Block(Seq(
          P.Var(l('x)), P.Var(l('y))
        )), ExpectedType.Undefined, Ctxt.terms(l('x) -> imm(U8), l('y) -> imm(U16))) ==> Right(U16)

        solveType(P.Block(Seq(
          P.ValDef(l('x), None, P.IntLit(10)),
          P.InfixAp(InfixOp.Add, P.Var(l('x)), P.IntLit(1))
        )), ExpectedType.Undefined, Ctxt.default) ==> Right(ast.Block(Seq(
          ast.ValDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.IntLit(1) :: I32) :: I32
        )) :: I32)

        findType(P.Block(Seq(
          P.ValDef(l('x), Some(TypeName.Named(g('i64))), P.IntLit(10)),
          P.Var(l('x))
        )), ExpectedType.Numeric(None)) ==> Right(I64)

        solveType(P.Block(Seq(
          P.VarDef(l('x), None, P.IntLit(10)),
          P.Assign(P.Var(l('x)), None, P.IntLit(20)),
          P.Var(l('x))
        )), ExpectedType.Undefined, Ctxt.default) ==> Right(ast.Block(Seq(
          ast.VarDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.Assign(ast.Var(l('x)) :: I32, None, ast.IntLit(20) :: I32) :: U0,
          ast.Var(l('x)) :: I32
        )) :: I32)
      }
      "App" - {
        "Fun" - {
          findType(P.App(P.Var(g('foo)), Seq(P.IntLit(32))), ExpectedType.Undefined,
            Ctxt.terms(g('foo) -> imm(Fun(Seq(I64), U1)))) ==> Right(U1)
        }
        "Ptr" - {
          "without offset" - {
            solveType(P.App(P.Var(l('foo)), Seq()), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('foo) -> Ctxt.Immutable(Ptr(TypeName.Named(g('i32))))))) ==>
              Right(ast.App(ast.Var(l('foo)) :: Ptr(TypeName.Named(g('i32))), Seq()) :: I32)
          }
          "with offset" - {
            solveType(P.App(P.Var(l('foo)), Seq(P.IntLit(20))), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('foo) -> Ctxt.Immutable(Ptr(TypeName.Named(g('i32))))))) ==>
              Right(ast.App(ast.Var(l('foo)) :: Ptr(TypeName.Named(g('i32))), Seq(ast.IntLit(20) :: I64)) :: I32)
          }
        }
      }
      "if expression" - {
        solveType(P.If(P.BoolLit(true), P.IntLit(10), Some(P.IntLit(20))),
          ExpectedType.Specific(I64), Ctxt.default) ==>
          Right(ast.If(ast.BoolLit(true) :: U1, ast.IntLit(10) :: I64, Some(ast.IntLit(20) :: I64)) :: I64)
        findType(P.If(P.BoolLit(true), P.IntLit(42), None), ExpectedType.Undefined) ==> Right(U0)
        findType(P.If(P.BoolLit(true), P.Var(l('x)), Some(P.Var(l('y)))),
          ExpectedType.Numeric(None), Ctxt.default.withTerms(Map(l('x) -> imm(I32), l('y) -> imm(I64)))) ==> Right(I64)
      }
      "While" - {
        solveType(P.While(
          P.InfixAp(InfixOp.Lt, P.Var(l('x)), P.IntLit(10)),
          P.Assign(P.Var(l('x)), Some(InfixOp.Add), P.IntLit(1))
        ), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> Ctxt.Mutable(I32)))) ==>
          Right(ast.While(
            ast.InfixAp(InfixOp.Lt, ast.Var(l('x)) :: I32, ast.IntLit(10) :: I32) :: U1,
            ast.Assign(ast.Var(l('x)) :: I32, None,
              ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.IntLit(1) :: I32) :: I32) :: U0
          ) :: U0)
      }
      "struct member select" - {
        findType(P.Select(P.Var(l('x)), "x"), ExpectedType.Undefined,
          Ctxt.default.withTerms(Map(l('x) -> imm(Struct(g('s), Seq(StructMember("x", I32), StructMember("x", I64))))))) ==>
          Right(I32)
      }
      "Assign" - {
        "Var" - {
          "mutable" - {
            solveType(P.Assign(P.Var(l('x)), None, P.IntLit(20)), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> Ctxt.Mutable(I64)))) ==>
              Right(ast.Assign(ast.Var(l('x)) :: I64, None, ast.IntLit(20) :: I64) :: U0)
          }
          "immutable" - {
            solveType(P.Assign(P.Var(l('x)), None, P.IntLit(20)), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> Ctxt.Immutable(I64)))) ==>
              Left(TyperError.ImmutableAssign(l('x)))
          }
        }
        "Ptr" - {
          val intptr = Ptr(TypeName.Named(g('i32)))
          solveType(P.Assign(P.App(P.Var(l('foo)), Seq()), None, P.IntLit(20)), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('foo) -> Ctxt.Mutable(intptr)))) ==>
            Right(ast.Assign(ast.App(ast.Var(l('foo)) :: intptr, Seq()) :: I32, None, ast.IntLit(20) :: I32) :: U0)
        }
        "composite" - {
          solveType(P.Assign(P.Var(l('x)), Some(InfixOp.Add), P.IntLit(20)), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('x) -> Ctxt.Mutable(I64)))) ==>
            Right(ast.Assign(ast.Var(l('x)) :: I64, None, ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I64, ast.IntLit(20) :: I64) :: I64) :: U0)
        }
        "An rval" - {
          solveType(P.Assign(P.IntLit(2), Some(InfixOp.Add), P.IntLit(1)), ExpectedType.Undefined, Ctxt.default) ==>
            Left(TyperError.NotAnLVal(ast.IntLit(2) :: I32))
        }
      }
      "Extern" - {
        "explicitly typed" - {
          solveType(P.Extern(), ExpectedType.Specific(I32), Ctxt.default) ==>
            Right(ast.Extern() :: I32)
        }
        "type-inferred" - {
          solveType(P.Extern(), ExpectedType.Undefined, Ctxt.default) ==>
            Left(TyperError.ExternNoExplicitType())
        }
      }
    }
  }

}
