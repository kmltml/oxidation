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
        case ExpectedType.Numeric(None) | ExpectedType.Undefined => Right(Typed(ast.IntLit(i), I32))
        case ExpectedType.Numeric(Some(t)) => Right(Typed(ast.IntLit(i), t))
        case ExpectedType.Specific(t @ (I8 | I16 | I32 | I64 | U8 | U16 | U32 | U64)) => Right(Typed(ast.IntLit(i), t))
        case _ => unifyType(Typed(ast.IntLit(i), I32), expected)
      }

    case P.BoolLit(b) => unifyType(Typed(ast.BoolLit(b), U1), expected)

    case P.CharLit(c) => unifyType(Typed(ast.CharLit(c), U8), expected)

    case P.StringLit(s) => unifyType(Typed(ast.StringLit(s), BuiltinSymbols.StrType), expected)

    case P.UnitLit() => unifyType(Typed(ast.UnitLit(), U0), expected)

    case P.Extern() => expected match {
      case ExpectedType.Specific(t) => Right(Typed(ast.Extern(), t))
      case _ => Left(TyperError.ExternNoExplicitType())
    }

    case P.App(P.TypeApp(P.Var(Symbol.Global(List("cast"))), List(target)), List(src)) =>
      solveType(src, ExpectedType.Undefined, ctxt).map(cast(target, _, ctxt))

    case P.TypeApp(P.Var(Symbol.Global(List("stackalloc"))), List(pointee)) =>
      unifyType(Typed(ast.Stackalloc(lookupType(pointee, ctxt)), Ptr(pointee)), expected)

    case P.StructLit(name, members) =>
      for {
        struct <- lookupType(TypeName.Named(name), ctxt) match {
          case s: Struct => Right(s)
          case t => Left(TyperError.NotAStruct(t))
        }
        _ <- Either.cond(members.map(_._1).toSet == struct.members.map(_.name).toSet, (),
                          TyperError.WrongStructMembers(expected = struct.members.map(_.name).toSet, found = members.map(_._1).toSet))
        typesMap = struct.members.map(m => m.name -> m.typ).toMap
        typedMembers <- members.traverse {
          case (name, expr) =>
            val expectedType = ExpectedType.Specific(typesMap(name))
            solveType(expr, expectedType, ctxt).map(name -> _)
        }
      } yield Typed(ast.StructLit(name, typedMembers), struct)

    case P.PrefixAp(PrefixOp.Inv, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric(None), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Inv, rtyped), rtyped.typ), expected)
      } yield t

    case P.PrefixAp(PrefixOp.Neg, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric(None), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Neg, rtyped), signed(rtyped.typ)), expected)
      } yield t

    case P.PrefixAp(PrefixOp.Not, right) =>
      for {
        rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Not, rtyped), rtyped.typ), expected)
      } yield t

    case P.InfixAp(op, left, right) =>
      op match {
        case InfixOp.Add | InfixOp.Sub | InfixOp.Mul | InfixOp.Div |
             InfixOp.Mod | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Shl | InfixOp.Shr =>
          val lexpected = expected match {
            case ExpectedType.Numeric(Some(_)) => expected
            case ExpectedType.Specific(_) => expected
            case _ => ExpectedType.Numeric(None)
          }
          for {
            ltyped <- solveType(left, lexpected, ctxt)
            rtyped <- solveType(right, ExpectedType.Numeric(Some(ltyped.typ)), ctxt)
            lunified <- unifyType(ltyped, ExpectedType.Specific(rtyped.typ))
            t <- unifyType(Typed(ast.InfixAp(op, lunified, rtyped), rtyped.typ), expected)
          } yield t

        case InfixOp.Eq | InfixOp.Neq =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected = ltyped.typ match {
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => ExpectedType.Numeric(Some(ltyped.typ))
              case t => ExpectedType.Specific(t)
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, U1), expected)
          } yield t

        case InfixOp.Geq | InfixOp.Gt | InfixOp.Lt | InfixOp.Leq =>
          for {
            ltyped <- solveType(left, ExpectedType.Numeric(None), ctxt)
            rtyped <- solveType(right, ExpectedType.Numeric(Some(ltyped.typ)), ctxt)
            widerType = wider(ltyped.typ, rtyped.typ)
            lwidened = if (ltyped.typ == widerType) ltyped else Typed(ast.Widen(ltyped), widerType)
            rwidened = if (rtyped.typ == widerType) rtyped else Typed(ast.Widen(rtyped), widerType)
            t <- unifyType(Typed(ast.InfixAp(op, lwidened, rwidened): ast.Expression, U1), expected)
          } yield t

        case InfixOp.And | InfixOp.Or =>
          for {
            ltyped <- solveType(left, ExpectedType.Specific(U1), ctxt)
            rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped), U1), expected)
          } yield t

        case InfixOp.Xor =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected <- ltyped.typ match {
              case U1 => Right(ExpectedType.Specific(U1))
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => Right(ExpectedType.Numeric(Some(ltyped.typ)))
              case t => Left(TyperError.CantMatch(ExpectedType.Undefined, t))
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped): ast.Expression, U1), expected) // TODO support xor on integers
          } yield t
      }

    case P.App(fn, pars) =>
      for {
        fnTyped <- solveType(fn, ExpectedType.Appliable, ctxt)
        typedParams <- fnTyped.typ match {
          case Fun(paramTypes, _) =>
            if (paramTypes.size != pars.size) Left(TyperError.WrongNumberOfArguments(paramTypes.size, pars.size))
            else (pars zip paramTypes).traverse {
              case (e, t) => solveType(e, ExpectedType.Specific(t), ctxt)
            }
          case Ptr(_) =>
            pars match {
              case Nil => Right(Nil)
              case List(off) => solveType(off, ExpectedType.Specific(I64), ctxt).map(List(_))
              case s => Left(TyperError.TooManyParamsForPointerDereference(s.size))
            }
        }
        valType <- fnTyped.typ match {
          case Fun(_, retType) => Right(retType)
          case Ptr(pointee) => Right(lookupType(pointee, ctxt))
        }
        t <- unifyType(Typed(ast.App(fnTyped, typedParams), valType), expected)
      } yield t


    case P.Var(name) =>
      ctxt.terms.get(name)
        .toRight(TyperError.SymbolNotFound(name))
        .flatMap(t => unifyType(Typed(ast.Var(name), t.typ), expected))

    case P.Select(expr, member) =>
      for {
        exprTyped <- solveType(expr, ExpectedType.Undefined, ctxt)
        memberType <- exprTyped.typ match {
          case t @ Struct(_, ms) =>
            ms.find(_.name == member).map(_.typ).toRight(TyperError.MemberNotFound(member, t))
          case t =>
            Left(TyperError.MemberNotFound(member, t))
        }
        t <- unifyType(Typed(ast.Select(exprTyped, member), memberType), expected)
      } yield t

    case P.If(cond, pos, None) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        posTyped <- solveType(pos, ExpectedType.Undefined, ctxt)
        t <- unifyType(Typed(ast.If(condTyped, posTyped, None), U0), expected)
      } yield t

    case P.If(cond, pos, Some(neg)) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        posTyped <- solveType(pos, expected, ctxt)
        negExpected = (expected, posTyped.typ) match {
          case (ExpectedType.Specific(t), _) => expected
          case (ExpectedType.Numeric(t), _) => ExpectedType.Numeric(t)
          case (ExpectedType.Appliable, t) => ExpectedType.Specific(t)
          case (ExpectedType.Undefined, t) => ExpectedType.Specific(t)
        }
        negTyped <- solveType(neg, negExpected, ctxt)
        t = (posTyped.typ, negTyped.typ) match {
          case (a, b) if a == b => a
          case (a, b) => wider(a, b) // TODO case this for numeric types only
        }
        res <- unifyType(Typed(ast.If(condTyped, posTyped, Some(negTyped)), t), expected)
      } yield res

    case P.While(cond, body) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        bodyTyped <- solveType(body, ExpectedType.Undefined, ctxt)
        t <- unifyType(Typed(ast.While(condTyped, bodyTyped), U0), expected)
      } yield t

    case P.Block(Vector()) =>
      unifyType(Typed(ast.Block(Vector.empty), U0), expected)

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
            _ <- StateT.lift(Either.cond(Set(ExpectedType.Undefined, ExpectedType.Specific(U0)) contains ex, (), TyperError.CantMatch(ex, U0): TyperError))
          } yield Typed(ast.ValDef(name, tpe, typedVal): ast.BlockStatement, U0)

        case P.VarDef(name, tpe, value) => // TODO remove this ugly duplication
          for {
            c <- StateT.get[TyperResult, Ctxt]
            expectedType = tpe.map(t => ExpectedType.Specific(lookupType(t, c))).getOrElse(ExpectedType.Undefined)
            typedVal <- StateT.lift(solveType(value, expectedType, c))
            _ <- StateT.modify[TyperResult, Ctxt](_.withTerms(Map(name -> Ctxt.Mutable(typedVal.typ))))
            _ <- StateT.lift(Either.cond(Set(ExpectedType.Undefined, ExpectedType.Specific(U0)) contains ex, (), TyperError.CantMatch(ex, U0): TyperError))
          } yield Typed(ast.VarDef(name, tpe, typedVal): ast.BlockStatement, U0)
      }
      (for {
        body <- stmnts.init.toVector.traverse(stmnt(_, ExpectedType.Undefined))
        last <- stmnt(stmnts.last, expected)
        t <- StateT.lift(unifyType(Typed(ast.Block(body :+ last), last.typ), expected))
      } yield t).runA(ctxt)

    case P.Assign(l, Some(op), r) =>
      solveType(P.Assign(l, None, P.InfixAp(op, l, r)), expected, ctxt)

    case P.Assign(lval, None, rval) =>
      for {
        ltyped <- solveLVal(lval, ctxt)
        rtyped <- solveType(rval, ExpectedType.Specific(ltyped.typ), ctxt)
        t <- unifyType(Typed(ast.Assign(ltyped, None, rtyped), U0), expected)
      } yield t
  }

  private def solveLVal(e: P.Expression, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = e match {
    case P.Var(s) => ctxt.terms(s) match {
      case Ctxt.Mutable(t) => Right(Typed(ast.Var(s), t))
      case Ctxt.Immutable(_) => Left(TyperError.ImmutableAssign(s))
    }
    case P.Select(s, member) =>
      solveLVal(s, ctxt).flatMap {
        case src @ Typed(_, struct @ Struct(_, members)) =>
          members.find(_.name == member) map {
            case StructMember(_, typ) => Typed(ast.Select(src, member), typ)
          } toRight TyperError.MemberNotFound(member, struct)
      }
    case e => solveType(e, ExpectedType.Undefined, ctxt).flatMap {
      case lTyped @ Typed(ast.App(Typed(ptr, _: Ptr), _), _) => Right(lTyped)
      case e => Left(TyperError.NotAnLVal(e))
    }
  }

  private def cast(target: TypeName, src: Typed[ast.Expression], ctxt: Ctxt): Typed[ast.Expression] =
    (lookupType(target, ctxt), src.typ) match {
      case (a, b) if a == b => src
      case (t: Num, s: Num) =>
        val w = wider(t, s)
        if(w == t) widen(src, t)
        else trim(src, t)
      case (t @ Ptr(_), Ptr(_) | U64) => Typed(ast.Reinterpret(src), t)
    }

  def lookupType(t: TypeName, ctxt: Ctxt): Type = t match {
    case TypeName.App(TypeName.Named(Symbol.Global(List("ptr"))), List(pointee)) => Type.Ptr(pointee)
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
        val paramTypes: List[(Symbol, Ctxt.Term)] = newParams.getOrElse(Nil) map {
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

  private def unifyType(t: Typed[ast.Expression], expected: ExpectedType): Either[TyperError, Typed[ast.Expression]] = (t.typ, expected) match {
    case (_: Num, ExpectedType.Numeric(None)) => Right(t)
    case (_: Num, ExpectedType.Numeric(Some(lb))) =>
      val w = wider(t.typ, lb)
      Right(if(w == t.typ) t else widen(t, w))
    case (typ: Num, ExpectedType.Specific(exp: Num)) =>
      val w = wider(typ, exp)
      if(typ == exp) Right(t)
      else if(w == typ) Left(TyperError.CantMatch(expected, typ))
      else Right(widen(t, w))
    case (t, ExpectedType.Numeric(_)) => Left(TyperError.CantMatch(expected, t))

    case (f: Fun, ExpectedType.Appliable) => Right(t)
    case (p: Ptr, ExpectedType.Appliable) => Right(t)
    case (_, ExpectedType.Appliable) => Left(TyperError.CantMatch(expected, t.typ))

    case (typ, ExpectedType.Specific(U0))
      if typ != U0 => Right(Typed(ast.Ignore(t), U0))

    case (typ, ExpectedType.Specific(u)) =>
      if(typ == u) Right(t) else Left(TyperError.CantMatch(expected, typ))
    case (_, ExpectedType.Undefined) => Right(t)
  }

  private def wider(a: Type, b: Type): Type = {
    if(a == b) a
    else {
      val sign = isSigned(a) || isSigned(b)
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

  private def widen(expr: Typed[ast.Expression], t: Type): Typed[ast.Expression] = expr match {
    case Typed(l: ast.IntLit, _) => Typed(l, t)
    case Typed(ast.Widen(e), _) => widen(e, t)
    case _ => Typed(ast.Widen(expr), t)
  }

  private def trim(expr: Typed[ast.Expression], t: Type): Typed[ast.Expression] = expr match {
    case Typed(l: ast.IntLit, _) => Typed(l, t)
    case Typed(ast.Trim(e), _) => trim(e, t)
    case _ => Typed(ast.Trim(expr), t)
  }

  private def bitwidth(t: Type): Int = t match {
    case U1 => 1
    case U8 | I8 => 8
    case U16 | I16 => 16
    case U32 | I32 => 32
    case U64 | I64 => 64
  }

  private def signed(t: Type): Type = t match {
    case I8 | U8 => I8
    case I16 | U16 => I16
    case I32 | U32 => I32
    case I64 | U64 => I64
  }

  private def isSigned(t: Type): Boolean = t match {
    case U8 | U16 | U32 | U64 => false
    case I8 | I16 | I32 | I64 => true
  }

}
