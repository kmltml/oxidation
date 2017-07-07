package oxidation
package analyze

import parse.{Span, ast => P}
import Type._
import oxidation.analyze.Typer.solveType
import utest._
import cats._
import cats.data._
import cats.implicits._

import Validated.{valid, invalid, invalidNel}

object TyperTests extends TestSuite with SymbolSyntax with TypedSyntax {

  def findType(expr: P.Expression, expectedType: ExpectedType, ctxt: Ctxt = Ctxt.default): ValidatedNel[TyperError, Type] =
    solveType(expr, expectedType, ctxt).map(_.typ)

  def imm(t: Type): Ctxt.Term = Ctxt.Immutable(t)
  def mut(t: Type): Ctxt.Term = Ctxt.Mutable(t)

  val loc = Span(None, 0, 0)

  val tests = apply {
    "solveType" - {
      "int literals" - {
        findType(P.IntLit(20, loc), ExpectedType.Undefined, Ctxt.empty) ==> valid(I32)
        findType(P.IntLit(20, loc), ExpectedType.Numeric(None), Ctxt.empty) ==> valid(I32)
        findType(P.IntLit(20, loc), ExpectedType.Specific(U32), Ctxt.empty) ==> valid(U32)
      }
      "FloatLit" - {
        solveType(P.FloatLit(BigDecimal(0.1), loc), ExpectedType.Undefined, Ctxt.default) ==>
          valid(P.FloatLit(BigDecimal(0.1), loc) :: F64)
        solveType(P.FloatLit(BigDecimal(0.1), loc), ExpectedType.Specific(F32), Ctxt.default) ==>
          valid(P.FloatLit(BigDecimal(0.1), loc) :: F32)
        solveType(P.FloatLit(BigDecimal(0.1), loc), ExpectedType.Numeric(Some(F64)), Ctxt.default) ==>
          valid(P.FloatLit(BigDecimal(0.1), loc) :: F64)
      }
      "bool literals" - {
        findType(P.BoolLit(true, loc), ExpectedType.Undefined) ==> valid(U1)
        findType(P.BoolLit(false, loc), ExpectedType.Specific(U1)) ==> valid(U1)
      }
      "CharLit" - {
        solveType(P.CharLit('a', loc), ExpectedType.Undefined, Ctxt.default) ==> valid(ast.CharLit('a', loc) :: U8)
      }
      "struct literals" - {
        val Vec2 = Struct(g('Vec2),
          StructMember("x", I32), StructMember("y", I64)
        )
        solveType(P.StructLit(g('Vec2), List(
          "x" -> P.IntLit(10, loc), "y" -> P.IntLit(20, loc)
        ), loc), ExpectedType.Undefined, Ctxt.default.withTypes(Map(g('Vec2) -> Vec2))) ==>
          valid(ast.StructLit(g('Vec2), List(
            "x" -> (ast.IntLit(10, loc) :: I32), "y" -> (ast.IntLit(20, loc) :: I64)
          ), loc) :: Vec2)
      }
      "UnitLit" - {
        solveType(P.UnitLit(loc), ExpectedType.Undefined, Ctxt.default) ==> valid(ast.UnitLit(loc) :: U0)
      }
      "implicit unit conversion" - {
        solveType(P.IntLit(42, loc), ExpectedType.Specific(U0), Ctxt.default) ==>
          valid(ast.Ignore(ast.IntLit(42, loc) :: I32, loc) :: U0)
      }
      "operator expressions" - {
        findType(P.InfixAp(InfixOp.Add, P.IntLit(5, loc), P.IntLit(10, loc), loc), ExpectedType.Undefined, Ctxt.empty) ==>
          valid(I32)
        findType(P.InfixAp(InfixOp.Mul, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Numeric(None),
          Ctxt.terms(l('x) -> imm(U8), l('y) -> imm(U32))) ==> valid(U32)

        findType(P.InfixAp(InfixOp.Eq, P.IntLit(1, loc), P.IntLit(2, loc), loc), ExpectedType.Undefined, Ctxt.empty) ==> valid(U1)
        "Floating point operations" - {
          "f64 + f64 -> f64" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(F64), l('y) -> imm(F64)))) ==>
              valid(ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: F64, ast.Var(l('y), loc) :: F64, loc) :: F64)
          }
          "f64 <= f64 -> u1" - {
            solveType(P.InfixAp(InfixOp.Leq, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(F64), l('y) -> imm(F64)))) ==>
              valid(ast.InfixAp(InfixOp.Leq, ast.Var(l('x), loc) :: F64, ast.Var(l('y), loc) :: F64, loc) :: U1)
          }
        }
        "widening in arithmetic expressions" - {
          "i32 + i8 -> i32" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Numeric(None),
              Ctxt.default.withTerms(Map(l('x) -> imm(I32), l('y) -> imm(I8)))) ==>
              valid(ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.Widen(ast.Var(l('y), loc) :: I8, loc) :: I32, loc) :: I32)
          }
          "i8 + i32 -> i32" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(I8), l('y) -> imm(I32)))) ==>
              valid(ast.InfixAp(InfixOp.Add, ast.Widen(ast.Var(l('x), loc) :: I8, loc) :: I32, ast.Var(l('y), loc) :: I32, loc) :: I32)
          }
          "(i8 + i32): i64 -> i64" - {
            solveType(P.InfixAp(InfixOp.Add, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Specific(I64),
              Ctxt.default.withTerms(Map(l('x) -> imm(I8), l('y) -> imm(I32)))) ==>
              valid(ast.InfixAp(InfixOp.Add, ast.Widen(ast.Var(l('x), loc) :: I8, loc) :: I64, ast.Widen(ast.Var(l('y), loc) :: I32, loc) :: I64, loc) :: I64)
          }
        }
        "Short-Circuit Boolean Ops" - {
          "valid" - {
            solveType(P.InfixAp(InfixOp.And, P.Var(l('x), loc), P.Var(l('y), loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(U1), l('y) -> imm(U1)))) ==>
              valid(ast.InfixAp(InfixOp.And, ast.Var(l('x), loc) :: U1, ast.Var(l('y), loc) :: U1, loc) :: U1)
          }
          "invalid" - {
            val yloc = Span(None, 10, 11)
            solveType(P.InfixAp(InfixOp.Or, P.Var(l('x), loc), P.Var(l('y), yloc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(U1), l('y) -> imm(U16)))) ==>
              invalidNel(TyperError.CantMatch(ExpectedType.Specific(U1), U16, yloc))
          }
        }
      }
      "unary prefix operator expressions" - {
        findType(P.PrefixAp(PrefixOp.Inv, P.IntLit(64, loc), loc), ExpectedType.Numeric(None)) ==> valid(I32)
        findType(P.PrefixAp(PrefixOp.Neg, P.Var(l('x), loc), loc), ExpectedType.Undefined, Ctxt.terms(l('x) -> imm(U64))) ==> valid(I64)
        findType(P.PrefixAp(PrefixOp.Not, P.BoolLit(false, loc), loc), ExpectedType.Undefined) ==> valid(U1)
        "-f64 -> f64" - {
          solveType(P.PrefixAp(PrefixOp.Neg, P.Var(l('x), loc), loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('x) -> imm(F64)))) ==>
            valid(ast.PrefixAp(PrefixOp.Neg, ast.Var(l('x), loc) :: F64, loc) :: F64)
        }
      }
      "block expression" - {
        findType(P.Block(Vector(
          P.Var(l('x), loc), P.Var(l('y), loc)
        ), loc), ExpectedType.Undefined, Ctxt.terms(l('x) -> imm(U8), l('y) -> imm(U16))) ==> valid(U16)

        solveType(P.Block(Vector(
          P.ValDef(l('x), None, P.IntLit(10, loc)),
          P.InfixAp(InfixOp.Add, P.Var(l('x), loc), P.IntLit(1, loc), loc)
        ), loc), ExpectedType.Undefined, Ctxt.default) ==> valid(ast.Block(Vector(
          ast.ValDef(l('x), None, ast.IntLit(10, loc) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.IntLit(1, loc) :: I32, loc) :: I32
        ), loc) :: I32)

        findType(P.Block(Vector(
          P.ValDef(l('x), Some(TypeName.Named(g('i64))), P.IntLit(10, loc)),
          P.Var(l('x), loc)
        ), loc), ExpectedType.Numeric(None)) ==> valid(I64)

        solveType(P.Block(Vector(
          P.VarDef(l('x), None, P.IntLit(10, loc)),
          P.Assign(P.Var(l('x), loc), None, P.IntLit(20, loc), loc),
          P.Var(l('x), loc)
        ), loc), ExpectedType.Undefined, Ctxt.default) ==> valid(ast.Block(Vector(
          ast.VarDef(l('x), None, ast.IntLit(10, loc) :: I32) :: U0,
          ast.Assign(ast.Var(l('x), loc) :: I32, None, ast.IntLit(20, loc) :: I32, loc) :: U0,
          ast.Var(l('x), loc) :: I32
        ), loc) :: I32)
      }
      "App" - {
        "Fun" - {
          findType(P.App(P.Var(g('foo), loc), List(P.IntLit(32, loc)), loc), ExpectedType.Undefined,
            Ctxt.terms(g('foo) -> imm(Fun(List(I64), U1)))) ==> valid(U1)
        }
        "Ptr" - {
          "without offset" - {
            solveType(P.App(P.Var(l('foo), loc), Nil, loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('foo) -> imm(Ptr(I32))))) ==>
              valid(ast.App(ast.Var(l('foo), loc) :: Ptr(I32), Nil, loc) :: I32)
          }
          "with offset" - {
            solveType(P.App(P.Var(l('foo), loc), List(P.IntLit(20, loc)), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('foo) -> imm(Ptr(I32))))) ==>
              valid(ast.App(ast.Var(l('foo), loc) :: Ptr(I32), List(ast.IntLit(20, loc) :: I64), loc) :: I32)
          }
        }
        "Arr" - {
          solveType(P.App(P.Var(l('foo), loc), List(P.IntLit(10, loc)), loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('foo) -> imm(Arr(I32, 20))))) ==>
            valid(ast.App(ast.Var(l('foo), loc) :: Arr(I32, 20), List(ast.IntLit(10, loc) :: I64), loc) :: I32)
        }

        "sqrt intrinsic" - {
          "f32" - {
            solveType(P.App(P.Var(g('sqrt), loc), List(P.Var(l('x), loc)), loc), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F32)))) ==>
              valid(ast.App(ast.Var(g('sqrt), loc) :: Fun(List(F32), F32), List(ast.Var(l('x), loc) :: F32), loc) :: F32)
          }
          "f64" - {
            solveType(P.App(P.Var(g('sqrt), loc), List(P.Var(l('x), loc)), loc), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F64)))) ==>
              valid(ast.App(ast.Var(g('sqrt), loc) :: Fun(List(F64), F64), List(ast.Var(l('x), loc) :: F64), loc) :: F64)
          }
        }
      }
      "if expression" - {
        solveType(P.If(P.BoolLit(true, loc), P.IntLit(10, loc), Some(P.IntLit(20, loc)), loc),
          ExpectedType.Specific(I64), Ctxt.default) ==>
          valid(ast.If(ast.BoolLit(true, loc) :: U1, ast.IntLit(10, loc) :: I64, Some(ast.IntLit(20, loc) :: I64), loc) :: I64)
        findType(P.If(P.BoolLit(true, loc), P.IntLit(42, loc), None, loc), ExpectedType.Undefined) ==> valid(U0)
        findType(P.If(P.BoolLit(true, loc), P.Var(l('x), loc), Some(P.Var(l('y), loc)), loc),
          ExpectedType.Numeric(None), Ctxt.default.withTerms(Map(l('x) -> imm(I32), l('y) -> imm(I64)))) ==> valid(I64)
      }
      "While" - {
        solveType(P.While(
          P.InfixAp(InfixOp.Lt, P.Var(l('x), loc), P.IntLit(10, loc), loc),
          P.Assign(P.Var(l('x), loc), Some(InfixOp.Add), P.IntLit(1, loc), loc), loc
        ), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> mut(I32)))) ==>
          valid(ast.While(
            ast.InfixAp(InfixOp.Lt, ast.Var(l('x), loc) :: I32, ast.IntLit(10, loc) :: I32, loc) :: U1,
            ast.Assign(ast.Var(l('x), loc) :: I32, None,
              ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.IntLit(1, loc) :: I32, loc) :: I32, loc) :: U0, loc
          ) :: U0)
      }
      "Match" - {
        "i32" - {
          solveType(P.Match(
            P.Var(l('x), loc), List(
              P.Pattern.IntLit(0, loc) -> P.IntLit(0, loc),
              P.Pattern.Var(l('x), loc) -> P.InfixAp(InfixOp.Sub, P.Var(l('x), loc), P.IntLit(1, loc), loc)
            ), loc
          ), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(I32)))) ==>
            valid(ast.Match(
              ast.Var(l('x), loc) :: I32, List(
                (ast.Pattern.IntLit(0, loc) :: I32) -> (ast.IntLit(0, loc) :: I32),
                (ast.Pattern.Var(l('x), loc) :: I32) -> (ast.InfixAp(InfixOp.Sub, ast.Var(l('x), loc) :: I32, ast.IntLit(1, loc) :: I32, loc) :: I32)
              ), loc
            ) :: I32)
        }
        "bool" - {
          solveType(P.Match(
            P.Var(l('x), loc), List(
              P.Pattern.BoolLit(true, loc) -> P.IntLit(1, loc),
              P.Pattern.BoolLit(false, loc) -> P.IntLit(0, loc)
            ), loc
          ), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(U1)))) ==>
            valid(ast.Match(
              ast.Var(l('x), loc) :: U1, List(
                (ast.Pattern.BoolLit(true, loc) :: U1) -> (ast.IntLit(1, loc) :: I32),
                (ast.Pattern.BoolLit(false, loc) :: U1) -> (ast.IntLit(0, loc) :: I32)
              ), loc
            ) :: I32)
        }
        "nonexhaustive" - {
          val matchLoc = Span(None, 0, 10)
          "i32" - {
            solveType(P.Match(
              P.Var(l('x), loc), List(
                P.Pattern.IntLit(0, loc) -> P.IntLit(0, loc),
                P.Pattern.IntLit(1, loc) -> P.IntLit(2, loc)
              ), matchLoc
            ), ExpectedType.Specific(I32), Ctxt.default.withTerms(Map(l('x) -> imm(I32)))) ==>
              invalidNel(TyperError.NonexhaustivePatternMatch(MatchSet.Any(I32), matchLoc))
          }
          "i1" - {
            solveType(P.Match(
              P.Var(l('x), loc), List(
                P.Pattern.BoolLit(true, loc) -> P.IntLit(1, loc)
              ), matchLoc
            ), ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(U1)))) ==>
              invalidNel(TyperError.NonexhaustivePatternMatch(MatchSet.Bool(false), matchLoc))
          }
        }
      }
      "Select" - {
        val s = Struct(g('s), StructMember("x", I32), StructMember("y", I64))
        "struct" - {
          findType(P.Select(P.Var(l('x), loc), "x", loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('x) -> imm(s)))) ==>
            valid(I32)
        }
        "ptr to struct" - {
          solveType(P.Select(P.Var(l('x), loc), "y", loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('x) -> imm(Ptr(s))))) ==>
            valid(ast.Select(ast.Var(l('x), loc) :: Ptr(s), "y", loc) :: Ptr(I64))
        }
      }
      "Assign" - {
        "Var" - {
          "mutable" - {
            solveType(P.Assign(P.Var(l('x), loc), None, P.IntLit(20, loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> mut(I64)))) ==>
              valid(ast.Assign(ast.Var(l('x), loc) :: I64, None, ast.IntLit(20, loc) :: I64, loc) :: U0)
          }
          "immutable" - {
            val xloc = Span(None, 10, 20)
            solveType(P.Assign(P.Var(l('x), xloc), None, P.IntLit(20, loc), loc), ExpectedType.Undefined,
              Ctxt.default.withTerms(Map(l('x) -> imm(I64)))) ==>
              invalidNel(TyperError.ImmutableAssign(l('x), xloc)) // TODO maybe range on whole assign would be better?
          }
        }
        "Ptr" - {
          val intptr = Ptr(I32)
          solveType(P.Assign(P.App(P.Var(l('foo), loc), Nil, loc), None, P.IntLit(20, loc), loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('foo) -> mut(intptr)))) ==>
            valid(ast.Assign(ast.App(ast.Var(l('foo), loc) :: intptr, Nil, loc) :: I32, None, ast.IntLit(20, loc) :: I32, loc) :: U0)
        }
        "Select" - {
          val struct = Struct(g('foo),
            StructMember("x", I32),
            StructMember("y", I64)
          )
          "mutable" - {
            solveType(P.Assign(P.Select(P.Var(l('x), loc), "x", loc), None, P.IntLit(10, loc), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> mut(struct)))) ==>
              valid(ast.Assign(ast.Select(ast.Var(l('x), loc) :: struct, "x", loc) :: I32, None, ast.IntLit(10, loc) :: I32, loc) :: U0)
          }
          "immutable" - {
            val xloc = Span(None, 10, 30)
            solveType(P.Assign(P.Select(P.Var(l('x), xloc), "x", loc), None, P.IntLit(10, loc), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(struct)))) ==>
              invalidNel(TyperError.ImmutableAssign(l('x), xloc))
          }
        }
        "Arr app" - {
          solveType(P.Assign(P.App(P.Var(l('a), loc), List(P.IntLit(5, loc)), loc), None, P.IntLit(10, loc), loc),
            ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('a) -> mut(Arr(I32, 10))))) ==>
            valid(ast.Assign(ast.App(ast.Var(l('a), loc) :: Arr(I32, 10), List(ast.IntLit(5, loc) :: I64), loc) :: I32, None, ast.IntLit(10, loc) :: I32, loc) :: U0)
        }
        "composite" - {
          solveType(P.Assign(P.Var(l('x), loc), Some(InfixOp.Add), P.IntLit(20, loc), loc), ExpectedType.Undefined,
            Ctxt.default.withTerms(Map(l('x) -> mut(I64)))) ==>
            valid(ast.Assign(ast.Var(l('x), loc) :: I64, None, ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I64, ast.IntLit(20, loc) :: I64, loc) :: I64, loc) :: U0)
        }
        "An rval" - {
          val loc2 = Span(None, 30, 31)
          solveType(P.Assign(P.IntLit(2, loc2), Some(InfixOp.Add), P.IntLit(1, loc), loc), ExpectedType.Undefined, Ctxt.default) ==>
            invalidNel(TyperError.NotAnLVal(ast.IntLit(2, loc2) :: I32, loc2))
        }
      }
      "Extern" - {
        "explicitly typed" - {
          solveType(P.Extern(loc), ExpectedType.Specific(I32), Ctxt.default) ==>
            valid(ast.Extern(loc) :: I32)
        }
        "type-inferred" - {
          val externLoc = Span(None, 10, 15)
          solveType(P.Extern(externLoc), ExpectedType.Undefined, Ctxt.default) ==>
            invalidNel(TyperError.ExternNoExplicitType(externLoc))
        }
      }
      "Cast" - {
        "widen" - {
          solveType(P.App(P.TypeApp(P.Var(g('cast), loc), List(TypeName.Named(g('i64))), loc), List(P.Var(l('x), loc)), loc),
            ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(I8)))) ==>
            valid(ast.Widen(ast.Var(l('x), loc) :: I8, loc) :: I64)
        }
        "trim" - {
          solveType(P.App(P.TypeApp(P.Var(g('cast), loc), List(TypeName.Named(g('i8))), loc), List(P.Var(l('x), loc)), loc),
            ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(I64)))) ==>
            valid(ast.Trim(ast.Var(l('x), loc) :: I64, loc) :: I8)
        }
        "reinterpret" - {
          "pointer to pointer" - {
            solveType(P.App(P.TypeApp(P.Var(g('cast), loc),
              List(TypeName.ptr(TypeName.Named(g('i32)))), loc), List(P.Var(l('x), loc)), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(Ptr(I8))))) ==>
              valid(ast.Reinterpret(ast.Var(l('x), loc) :: Ptr(I8), loc) :: Ptr(I32))
          }
        }
        "convert" - {
          "f32 to i32" - {
            solveType(P.App(P.TypeApp(P.Var(g('cast), loc),
              List(TypeName.Named(g('i32))), loc), List(P.Var(l('x), loc)), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F32)))) ==>
              valid(ast.Convert(ast.Var(l('x), loc) :: F32, loc) :: I32)
          }
          "f64 to i32" - {
            solveType(P.App(P.TypeApp(P.Var(g('cast), loc),
              List(TypeName.Named(g('i32))), loc), List(P.Var(l('x), loc)), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F64)))) ==>
              valid(ast.Convert(ast.Var(l('x), loc) :: F64, loc) :: I32)
          }
          "f32 to i64" - {
            solveType(P.App(P.TypeApp(P.Var(g('cast), loc),
              List(TypeName.Named(g('i64))), loc), List(P.Var(l('x), loc)), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F32)))) ==>
              valid(ast.Convert(ast.Var(l('x), loc) :: F32, loc) :: I64)
          }
          "f64 to i64" - {
            solveType(P.App(P.TypeApp(P.Var(g('cast), loc),
              List(TypeName.Named(g('i64))), loc), List(P.Var(l('x), loc)), loc),
              ExpectedType.Undefined, Ctxt.default.withTerms(Map(l('x) -> imm(F64)))) ==>
              valid(ast.Convert(ast.Var(l('x), loc) :: F64, loc) :: I64)
          }
        }
      }
      "Stackalloc" - {
        solveType(P.TypeApp(P.Var(g('stackalloc), loc), List(TypeName.Named(g('i64))), loc), ExpectedType.Undefined, Ctxt.default) ==>
          valid(ast.Stackalloc(I64, loc) :: Ptr(I64))
      }
      "Array Literal" - {
        "fully explicit list" - {
          solveType(P.App(P.TypeApp(P.Var(g('arr), loc), List(TypeName.Named(g('i32)), TypeName.IntLiteral(5)), loc), List(1l, 2l, 3l, 4l, 5l).map(P.IntLit(_, loc)), loc),
            ExpectedType.Undefined, Ctxt.default) ==>
            valid(ast.ArrLit(List(1l, 2l, 3l, 4l, 5l).map(ast.IntLit(_, loc) :: I32), loc) :: Arr(I32, 5))
        }
        "fully explicit fill" - {
          solveType(P.App(P.TypeApp(P.Var(g('arr), loc), List(TypeName.Named(g('i32)), TypeName.IntLiteral(5)), loc), List(P.IntLit(1, loc)), loc),
            ExpectedType.Undefined, Ctxt.default) ==>
            valid(ast.ArrLit(List(ast.IntLit(1, loc) :: I32), loc) :: Arr(I32, 5))
        }
        "implicit size" - {
          solveType(P.App(P.TypeApp(P.Var(g('arr), loc), List(TypeName.Named(g('i32))), loc), List(1l, 2l, 3l, 4l, 5l).map(P.IntLit(_, loc)), loc),
            ExpectedType.Undefined, Ctxt.default) ==>
            valid(ast.ArrLit(List(1l, 2l, 3l, 4l, 5l).map(ast.IntLit(_, loc) :: I32), loc) :: Arr(I32, 5))
        }
      }
    }
  }

}
