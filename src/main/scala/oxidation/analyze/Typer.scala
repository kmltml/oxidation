package oxidation
package analyze

import parse.{ast => P}
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

    case P.CharLit(c) => unifyType(U8, expected).map(Typed(ast.CharLit(c), _))

    case P.StructLit(name, members) =>
      for {
        struct <- lookupType(TypeName.Named(name), ctxt) match {
          case s: Struct => Right(s)
          case t => Left(TyperError.NotAStruct(t))
        }
        _ <- Either.cond(members.map(_._1).toSet == struct.members.map(_.name).toSet, (),
                          TyperError.WrongStructMembers(expected = struct.members.map(_.name).toSet, found = members.map(_._1).toSet))
        typesMap = struct.members.map(m => m.name -> m.typ).toMap
        typedMembers <- members.toVector.traverse {
          case (name, expr) =>
            val expectedType = ExpectedType.Specific(typesMap(name))
            solveType(expr, expectedType, ctxt).map(name -> _)
        }
      } yield Typed(ast.StructLit(name, typedMembers), struct)

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
          case Ptr(_) =>
            pars match {
              case Seq() => Right(Seq.empty)
              case Seq(off) => solveType(off, ExpectedType.Numeric, ctxt).map(Seq(_))
              case s => Left(TyperError.TooManyParamsForPointerDereference(s.size))
            }
        }
        valType <- fnTyped.typ match {
          case Fun(_, retType) => Right(retType)
          case Ptr(pointee) => Right(lookupType(pointee, ctxt))
        }
        t <- unifyType(valType, expected)
      } yield Typed(ast.App(fnTyped, typedParams), t)


    case P.Var(name) =>
      ctxt.terms.get(name)
        .map(t => Typed(ast.Var(name): ast.Expression, t.typ))
        .toRight(TyperError.SymbolNotFound(name))

    case P.Select(expr, member) =>
      for {
        exprTyped <- solveType(expr, ExpectedType.Undefined, ctxt)
        memberType <- exprTyped.typ match {
          case t @ Struct(_, ms) =>
            ms.find(_.name == member).map(_.typ).toRight(TyperError.MemberNotFound(member, t))
          case t =>
            Left(TyperError.MemberNotFound(member, t))
        }
        t <- unifyType(memberType, expected)
      } yield Typed(ast.Select(exprTyped, member), t)

    case P.If(cond, pos, None) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        posTyped <- solveType(pos, ExpectedType.Undefined, ctxt)
        t <- unifyType(U0, expected)
      } yield Typed(ast.If(condTyped, posTyped, None), t)

    case P.If(cond, pos, Some(neg)) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        posTyped <- solveType(pos, expected, ctxt)
        negExpected = (expected, posTyped.typ) match {
          case (ExpectedType.Specific(t), _) => expected
          case (ExpectedType.Numeric, _) => ExpectedType.Numeric
          case (ExpectedType.Appliable, t) => ExpectedType.Specific(t)
          case (ExpectedType.Undefined, t) => ExpectedType.Specific(t)
        }
        negTyped <- solveType(neg, negExpected, ctxt)
        t = (posTyped.typ, negTyped.typ) match {
          case (a, b) if a == b => a
          case (a, b) => wider(a, b) // TODO case this for numeric types only
        }
        typ <- unifyType(t, expected)
      } yield Typed(ast.If(condTyped, posTyped, Some(negTyped)), typ)

    case P.While(cond, body) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        bodyTyped <- solveType(body, ExpectedType.Undefined, ctxt)
        t <- unifyType(U0, expected)
      } yield Typed(ast.While(condTyped, bodyTyped), t)

    case P.Block(Seq()) =>
      Right(Typed(ast.Block(Seq.empty), U0))

    case P.Block(stmnts) =>
      type S[A] = StateT[TyperResult, Ctxt, A]
      def stmnt(s: P.BlockStatement, ex: ExpectedType): S[Typed[ast.BlockStatement]] = s match {
        case e: P.Expression =>
          for {
            c <- StateT.get[TyperResult, Ctxt]
            t <- StateT.lift(solveType(e, ex, c))
          } yield t

        case P.ValDef(name, tpe, value) =>
          for {
            c <- StateT.get[TyperResult, Ctxt]
            expectedType = tpe.map(t => ExpectedType.Specific(lookupType(t, c))).getOrElse(ExpectedType.Undefined)
            typedVal <- StateT.lift(solveType(value, expectedType, c))
            _ <- StateT.modify[TyperResult, Ctxt](_.withTerms(Map(name -> Ctxt.Immutable(typedVal.typ))))
            t <- StateT.lift(unifyType(U0, ex))
          } yield Typed(ast.ValDef(name, tpe, typedVal): ast.BlockStatement, t)

        case P.VarDef(name, tpe, value) => // TODO remove this ugly duplication
          for {
            c <- StateT.get[TyperResult, Ctxt]
            expectedType = tpe.map(t => ExpectedType.Specific(lookupType(t, c))).getOrElse(ExpectedType.Undefined)
            typedVal <- StateT.lift(solveType(value, expectedType, c))
            _ <- StateT.modify[TyperResult, Ctxt](_.withTerms(Map(name -> Ctxt.Mutable(typedVal.typ))))
            t <- StateT.lift(unifyType(U0, ex))
          } yield Typed(ast.VarDef(name, tpe, typedVal): ast.BlockStatement, t)
      }
      (for {
        body <- stmnts.init.toVector.traverse(stmnt(_, ExpectedType.Undefined))
        last <- stmnt(stmnts.last, expected)
        t <- StateT.lift(unifyType(last.typ, expected))
      } yield Typed(ast.Block(body :+ last), last.typ)).runA(ctxt)

    case P.Assign(l, Some(op), r) =>
      solveType(P.Assign(l, None, P.InfixAp(op, l, r)), expected, ctxt)

    case P.Assign(lval, None, rval) =>
      for {
        ltyped <- lval match {
          case P.Var(s) => ctxt.terms(s) match {
            case Ctxt.Mutable(t) => Right(Typed(ast.Var(s), t))
            case Ctxt.Immutable(_) => Left(TyperError.ImmutableAssign(s))
          }
          case e => solveType(e, ExpectedType.Undefined, ctxt).flatMap {
            case lTyped @ Typed(ast.App(Typed(ptr, _: Ptr), _), _) => Right(lTyped)
            case e => Left(TyperError.NotAnLVal(e))
          }
        }
        rtyped <- solveType(rval, ExpectedType.Specific(ltyped.typ), ctxt)
        t <- unifyType(U0, expected)
      } yield Typed(ast.Assign(ltyped, None, rtyped), t)
  }

  def lookupType(t: TypeName, ctxt: Ctxt): Type = t match {
    case TypeName.App(TypeName.Named(Symbol.Global(Seq("ptr"))), Seq(pointee)) => Type.Ptr(pointee)
    case TypeName.Named(s) => ctxt.types(s)
  }

  def solveTermDef(d: P.TermDef, ctxt: Ctxt): TyperResult[ast.TermDef] = {
    val expectedType = d.typ.map(ExpectedType.Specific compose (lookupType(_, ctxt)))
      .getOrElse(ExpectedType.Undefined)
    d match {
      case P.DefDef(name, params, tpe, body) =>
        val newParams = params.map(_.map {
          case P.Param(name, tpe) => ast.Param(name, lookupType(tpe, ctxt))
        })
        val paramTypes: Seq[(Symbol, Ctxt.Term)] = newParams.getOrElse(Seq.empty) map {
          case ast.Param(name, tpe) => Symbol.Local(name) -> Ctxt.Immutable(tpe)
        }
        val localCtxt = ctxt.withTerms(paramTypes.toMap)
        solveType(body, expectedType, localCtxt)
          .map(ast.DefDef(name, newParams, tpe, _))

      case P.ValDef(name, tpe, value) =>
        solveType(value, expectedType, ctxt)
          .map(ast.ValDef(name, tpe, _))

      case P.VarDef(name, tpe, value) =>
        solveType(value, expectedType, ctxt)
          .map(ast.VarDef(name, tpe, _))
    }
  }

  private def unifyType(t: Type, expected: ExpectedType): Either[TyperError, Type] = (t, expected) match {
    case (I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64, ExpectedType.Numeric) => Right(t)
    case (t, ExpectedType.Numeric) => Left(TyperError.CantMatch(expected, t))

    case (f: Fun, ExpectedType.Appliable) => Right(f)
    case (p: Ptr, ExpectedType.Appliable) => Right(p)
    case (_, ExpectedType.Appliable) => Left(TyperError.CantMatch(expected, t))

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
