package oxidation
package analyze

import parse.{ ast => P }
import Type._

import cats._
import cats.data._
import cats.implicits._

object Typer {

  type TyperResult[A] = Either[TyperError, A]

  def solveType(expression: P.Expression, expected: ExpectedType, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = expression match {
    case P.IntLit(i) =>
      expected match {
        case ExpectedType.Numeric | ExpectedType.Undefined => Right(Typed(ast.IntLit(i), I32))
        case ExpectedType.Specific(t @ (I8 | I16 | I32 | I64)) => Right(Typed(ast.IntLit(i), t))
        case ExpectedType.Specific(t @ (U8 | U16 | U32 | U64)) =>
          if(i >= 0) Right(Typed(ast.IntLit(i), t)) else Left(TyperError.CantMatch(expected, I32))
        case _ => Left(TyperError.CantMatch(expected, I32))
      }

    case P.BoolLit(b) => unifyType(U1, expected).map(Typed(ast.BoolLit(b), _))

    case P.PrefixAp(PrefixOp.Inv, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric, ctxt)
        t <- unifyType(rtyped.typ, expected)
      } yield Typed(ast.PrefixAp(PrefixOp.Inv, rtyped), t)

    case P.PrefixAp(PrefixOp.Neg, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric, ctxt)
        t <- unifyType(wider(rtyped.typ, I8), expected)
      } yield Typed(ast.PrefixAp(PrefixOp.Neg, rtyped), t)

    case P.PrefixAp(PrefixOp.Not, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
        t <- unifyType(rtyped.typ, expected)
      } yield Typed(ast.PrefixAp(PrefixOp.Not, rtyped), t)

    case P.InfixAp(op, left, right) =>
      op match {
        case InfixOp.Add | InfixOp.Sub | InfixOp.Mul | InfixOp.Div |
             InfixOp.Mod | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Shl | InfixOp.Shr =>
          for {
            ltyped <- solveType(left, ExpectedType.Numeric, ctxt)
            rtyped <- solveType(right, ExpectedType.Numeric, ctxt)
            t <- unifyType(wider(ltyped.typ, rtyped.typ), expected)
          } yield Typed(ast.InfixAp(op, ltyped, rtyped), t)

        case InfixOp.Eq | InfixOp.Neq =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected = ltyped.typ match {
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => ExpectedType.Numeric
              case t => ExpectedType.Specific(t)
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(U1, expected)
          } yield Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, t)

        case InfixOp.Geq | InfixOp.Gt | InfixOp.Lt | InfixOp.Leq =>
          for {
            ltyped <- solveType(left, ExpectedType.Numeric, ctxt)
            rtyped <- solveType(right, ExpectedType.Numeric, ctxt)
            t <- unifyType(U1, expected)
          } yield Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, t)

        case InfixOp.And | InfixOp.Or =>
          for {
            ltyped <- solveType(left, ExpectedType.Specific(U1), ctxt)
            rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
            t <- unifyType(U1, expected)
          } yield Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, t)

        case InfixOp.Xor =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected <- ltyped.typ match {
              case U1 => Right(ExpectedType.Specific(U1))
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => Right(ExpectedType.Numeric)
              case t => Left(TyperError.CantMatch(ExpectedType.Undefined, t))
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(U1, expected)
          } yield Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, t)
      }

    case P.App(fn, pars) =>
      for {
        fnTyped <- solveType(fn, ExpectedType.Appliable, ctxt)
        typedParams <- fnTyped.typ match {
          case Fun(paramTypes, _) =>
            if (paramTypes.size != pars.size) Left(TyperError.WrongNumberOfArguments(paramTypes.size, pars.size))
            else (pars zip paramTypes).toVector.traverse {
              case (e, t) => solveType(e, ExpectedType.Specific(t), ctxt)
            }
        }
        valType <- fnTyped.typ match {
          case Fun(_, retType) => Right(retType)
        }
        t <- unifyType(valType, expected)
      } yield Typed(ast.App(fnTyped, typedParams), t)


    case P.Var(name) =>
      ctxt.terms.get(name)
        .map(Typed(ast.Var(name): ast.Expression, _))
        .toRight(TyperError.SymbolNotFound(name))

    case P.Block(stmnts) =>
      type S[A] = StateT[TyperResult, Ctxt, A]
      stmnts.toList.traverse[S, Typed[ast.Expression]] {
        case e: P.Expression =>
          for {
            c <- StateT.get[TyperResult, Ctxt]
            t <- StateT.lift(solveType(e, ExpectedType.Undefined, c))
          } yield t

        case P.ValDef(name, tpe, value) =>
          for {
            c <- StateT.get[TyperResult, Ctxt]
            expectedType = tpe.map(t => ExpectedType.Specific(lookupType(t, c))).getOrElse(ExpectedType.Undefined)
            t <- StateT.lift(solveType(value, expectedType, c))
            _ <- StateT.modify[TyperResult, Ctxt](_.withTerms(Map(name -> t.typ)))
          } yield t

      }.runA(ctxt).map { body =>
        val lastType = body.lastOption.map(_.typ).getOrElse(U0)
        Typed(ast.Block(body), lastType)
      }
  }

  def lookupType(t: TypeName, ctxt: Ctxt): Type = t match {
    case TypeName.Named(Symbol.Global(Seq(n))) => n match {
      case "unit" => U0
      case "bool" => U1
      case "u8" => U8
      case "u16" => U16
      case "u32" => U32
      case "u64" => U64
      case "i8" => I8
      case "i16" => I16
      case "i32" => I32
      case "i64" => I64
    }
    case TypeName.Named(s) => ctxt.types(s)
  }

  def solveTermDef(d: P.TermDef, ctxt: Ctxt): TyperResult[ast.TermDef] = {
    val expectedType = d.typ.map(ExpectedType.Specific compose (lookupType(_, ctxt)))
      .getOrElse(ExpectedType.Undefined)
    d match {
      case P.DefDef(name, params, tpe, body) =>
        val paramTypes = params.getOrElse(Seq.empty) map {
          case Param(name, tpe) => Symbol.Local(name) -> lookupType(tpe, ctxt)
        }
        val localCtxt = ctxt.withTerms(paramTypes.toMap)
        solveType(body, expectedType, localCtxt)
          .map(ast.DefDef(name, params, tpe, _))

      case P.ValDef(name, tpe, value) =>
        solveType(value, expectedType, ctxt)
          .map(ast.ValDef(name, tpe, _))

      case P.VarDef(name, tpe, value) =>
        solveType(value, expectedType, ctxt)
          .map(ast.ValDef(name, tpe, _))
    }
  }

  private def unifyType(t: Type, expected: ExpectedType): Either[TyperError, Type] = (t, expected) match {
    case (I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64, ExpectedType.Numeric) => Right(t)
    case (t, ExpectedType.Numeric) => Left(TyperError.CantMatch(expected, t))
    case (t, ExpectedType.Specific(u)) =>
      if(t == u) Right(t) else Left(TyperError.CantMatch(expected, t))
    case (t, ExpectedType.Undefined) => Right(t)
  }

  private def wider(a: Type, b: Type): Type = {
    if(a == b) a
    else {
      val sign = signed(a) || signed(b)
      val width = bitwidth(a) max bitwidth(b)
      (sign, width) match {
        case (true, 8)   => I8
        case (true, 16)  => I16
        case (true, 32)  => I32
        case (true, 64)  => I64
        case (false, 8)  => U8
        case (false, 16) => U16
        case (false, 32) => U32
        case (false, 64) => U64
      }
    }
  }

  private def bitwidth(t: Type): Int = t match {
    case U1 => 1
    case U8 | I8 => 8
    case U16 | I16 => 16
    case U32 | I32 => 32
    case U64 | I64 => 64
  }

  private def signed(t: Type): Boolean = t match {
    case U8 | U16 | U32 | U64 => false
    case I8 | I16 | I32 | I64 => true
  }

}
