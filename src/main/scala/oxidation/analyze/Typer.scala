package oxidation
package analyze

import parse.{Span, ast => P}
import Type._
import cats._
import cats.data._
import cats.implicits._

object Typer {

  type TyperResult[A] = Either[TyperError, A]

  def solveType(expression: P.Expression, expected: ExpectedType, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = expression match {
    case P.IntLit(i, loc) =>
      expected match {
        case ExpectedType.Numeric(None) | ExpectedType.Undefined => Right(Typed(ast.IntLit(i, loc), I32))
        case ExpectedType.Numeric(Some(t: Integral)) => Right(Typed(ast.IntLit(i, loc), t))
        case ExpectedType.Specific(t: Integral) => Right(Typed(ast.IntLit(i, loc), t))
        case _ => unifyType(Typed(ast.IntLit(i, loc), I32), expected)
      }

    case P.FloatLit(f, loc) =>
      expected match {
        case ExpectedType.Numeric(None) | ExpectedType.Undefined => Right(Typed(ast.FloatLit(f, loc), F64))
        case ExpectedType.Numeric(Some(t: F)) => Right(Typed(ast.FloatLit(f, loc), t))
        case ExpectedType.Specific(t: F) => Right(Typed(ast.FloatLit(f, loc), t))
        case _ => unifyType(Typed(ast.FloatLit(f, loc), F64), expected)
      }

    case P.BoolLit(b, loc) => unifyType(Typed(ast.BoolLit(b, loc), U1), expected)

    case P.CharLit(c, loc) => unifyType(Typed(ast.CharLit(c, loc), U8), expected)

    case P.StringLit(s, loc) => unifyType(Typed(ast.StringLit(s, loc), BuiltinSymbols.StrType), expected)

    case P.UnitLit(loc) => unifyType(Typed(ast.UnitLit(loc), U0), expected)

    case P.Extern(loc) => expected match {
      case ExpectedType.Specific(t) => Right(Typed(ast.Extern(loc), t))
      case _ => Left(TyperError.ExternNoExplicitType(loc))
    }

    case P.App(P.TypeApp(P.Var(Symbol.Global(List("cast")), _), List(target), _), List(src), loc) =>
      solveType(src, ExpectedType.Undefined, ctxt).map(cast(target, _, loc, ctxt))

    case P.TypeApp(P.Var(Symbol.Global(List("stackalloc")), _), List(pointee), loc) =>
      unifyType(Typed(ast.Stackalloc(lookupType(pointee, ctxt), loc), Ptr(lookupType(pointee, ctxt))), expected)

    case P.App(P.TypeApp(P.Var(Symbol.Global(List("arr")), _), membertn :: typeTail, _), termParams, loc) =>
      for {
        size <- typeTail.headOption traverse {
          case TypeName.IntLiteral(i) => Right(i.toInt)
          case tn => Left(TyperError.NotASingletonType(tn))
        }
        _ <- size match {
          case Some(s) => Either.cond(termParams.size == s || termParams.size == 1, (), TyperError.WrongNumberOfArguments(s, termParams.size, loc))
          case None => Right(())
        }
        memberType = lookupType(membertn, ctxt)
        typedParams <- termParams.traverse(solveType(_, ExpectedType.Specific(memberType), ctxt))
      } yield Typed(ast.ArrLit(typedParams, loc), Arr(memberType, size getOrElse typedParams.size))

    case P.App(P.Var(sqrt @ Symbol.Global(List("sqrt")), sqrtloc), List(param), loc) =>
      for {
        typedParam <- solveType(param, ExpectedType.Numeric(None), ctxt)
        pt = typedParam.typ
        _ <- Either.cond(pt.isInstanceOf[Type.F], (), TyperError.CantMatch(ExpectedType.Specific(Type.F64), pt, param.loc))
      } yield Typed(ast.App(Typed(ast.Var(sqrt, sqrtloc), Type.Fun(List(pt), pt)), List(typedParam), loc), pt)

    case P.StructLit(name, members, loc) =>
      for {
        struct <- lookupType(TypeName.Named(name), ctxt) match {
          case s: Struct => Right(s)
          case t => Left(TyperError.NotAStruct(t, loc))
        }
        _ <- Either.cond(members.map(_._1).toSet == struct.members.map(_.name).toSet, (),
                          TyperError.WrongStructMembers(expected = struct.members.map(_.name).toSet, found = members.map(_._1).toSet, loc))
        typesMap = struct.members.map(m => m.name -> m.typ).toMap
        typedMembers <- members.traverse {
          case (name, expr) =>
            val expectedType = ExpectedType.Specific(typesMap(name))
            solveType(expr, expectedType, ctxt).map(name -> _)
        }
      } yield Typed(ast.StructLit(name, typedMembers, loc), struct)

    case P.PrefixAp(PrefixOp.Inv, right, loc) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric(None), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Inv, rtyped, loc), rtyped.typ), expected)
      } yield t

    case P.PrefixAp(PrefixOp.Neg, right, loc) =>
      for {
        rtyped <- solveType(right, ExpectedType.Numeric(None), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Neg, rtyped, loc), signed(rtyped.typ)), expected)
      } yield t

    case P.PrefixAp(PrefixOp.Not, right, loc) =>
      for {
        rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
        t <- unifyType(Typed(ast.PrefixAp(PrefixOp.Not, rtyped, loc), rtyped.typ), expected)
      } yield t

    case P.InfixAp(op, left, right, loc) =>
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
            t <- unifyType(Typed(ast.InfixAp(op, lunified, rtyped, loc), rtyped.typ), expected)
          } yield t

        case InfixOp.Eq | InfixOp.Neq =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected = ltyped.typ match {
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => ExpectedType.Numeric(Some(ltyped.typ))
              case t => ExpectedType.Specific(t)
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped, loc): ast.Expression, U1), expected)
          } yield t

        case InfixOp.Geq | InfixOp.Gt | InfixOp.Lt | InfixOp.Leq =>
          for {
            ltyped <- solveType(left, ExpectedType.Numeric(None), ctxt)
            rtyped <- solveType(right, ExpectedType.Numeric(Some(ltyped.typ)), ctxt)
            widerType = wider(ltyped.typ, rtyped.typ)
            lwidened = if (ltyped.typ == widerType) ltyped else Typed(ast.Widen(ltyped, ltyped.expr.loc), widerType)
            rwidened = if (rtyped.typ == widerType) rtyped else Typed(ast.Widen(rtyped, rtyped.expr.loc), widerType)
            t <- unifyType(Typed(ast.InfixAp(op, lwidened, rwidened, loc): ast.Expression, U1), expected)
          } yield t

        case InfixOp.And | InfixOp.Or =>
          for {
            ltyped <- solveType(left, ExpectedType.Specific(U1), ctxt)
            rtyped <- solveType(right, ExpectedType.Specific(U1), ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped, loc), U1), expected)
          } yield t

        case InfixOp.Xor =>
          for {
            ltyped <- solveType(left, ExpectedType.Undefined, ctxt)
            rightExpected <- ltyped.typ match {
              case U1 => Right(ExpectedType.Specific(U1))
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => Right(ExpectedType.Numeric(Some(ltyped.typ)))
              case t => Left(TyperError.CantMatch(ExpectedType.Undefined, t, left.loc))
            }
            rtyped <- solveType(right, rightExpected, ctxt)
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped, loc): ast.Expression, U1), expected) // TODO support xor on integers
          } yield t
      }

    case P.App(fn, pars, loc) =>
      for {
        fnTyped <- solveType(fn, ExpectedType.Appliable, ctxt)
        typedParams <- fnTyped.typ match {
          case Fun(paramTypes, _) =>
            if (paramTypes.size != pars.size) Left(TyperError.WrongNumberOfArguments(paramTypes.size, pars.size, loc))
            else (pars zip paramTypes).traverse {
              case (e, t) => solveType(e, ExpectedType.Specific(t), ctxt)
            }
          case Ptr(_) =>
            pars match {
              case Nil => Right(Nil)
              case List(off) => solveType(off, ExpectedType.Specific(I64), ctxt).map(List(_))
              case s => Left(TyperError.TooManyParamsForPointerDereference(s.size, loc))
            }
          case Arr(_, _) =>
            if(pars.size != 1) Left(TyperError.WrongNumberOfArguments(1, pars.size, loc))
            else pars.traverse(solveType(_, ExpectedType.Specific(I64), ctxt))
        }
        valType <- fnTyped.typ match {
          case Fun(_, retType) => Right(retType)
          case Ptr(pointee) => Right(pointee)
          case Arr(member, _) => Right(member)
        }
        t <- unifyType(Typed(ast.App(fnTyped, typedParams, loc), valType), expected)
      } yield t


    case P.Var(name, loc) =>
      ctxt.terms.get(name)
        .toRight(TyperError.SymbolNotFound(name, loc))
        .flatMap(t => unifyType(Typed(ast.Var(name, loc), t.typ), expected))

    case P.Select(expr, member, loc) =>
      for {
        exprTyped <- solveType(expr, ExpectedType.Undefined, ctxt)
        memberType <- exprTyped.typ match {
          case t @ Struct(_, ms) =>
            ms.find(_.name == member).map(_.typ).toRight(TyperError.MemberNotFound(member, t, loc))
          case Ptr(s @ Struct(_, ms)) =>
            ms.find(_.name == member).map(x => Ptr(x.typ)).toRight(TyperError.MemberNotFound(member, s, loc))
          case t =>
            Left(TyperError.MemberNotFound(member, t, loc))
        }
        t <- unifyType(Typed(ast.Select(exprTyped, member, loc), memberType), expected)
      } yield t

    case P.If(cond, pos, None, loc) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        posTyped <- solveType(pos, ExpectedType.Undefined, ctxt)
        t <- unifyType(Typed(ast.If(condTyped, posTyped, None, loc), U0), expected)
      } yield t

    case P.If(cond, pos, Some(neg), loc) =>
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
        res <- unifyType(Typed(ast.If(condTyped, posTyped, Some(negTyped), loc), t), expected)
      } yield res

    case P.While(cond, body, loc) =>
      for {
        condTyped <- solveType(cond, ExpectedType.Specific(U1), ctxt)
        bodyTyped <- solveType(body, ExpectedType.Undefined, ctxt)
        t <- unifyType(Typed(ast.While(condTyped, bodyTyped, loc), U0), expected)
      } yield t

    case P.Block(Vector(), loc) =>
      unifyType(Typed(ast.Block(Vector.empty, loc), U0), expected)

    case P.Block(stmnts, loc) =>
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
            _ <- StateT.lift(Either.cond(Set(ExpectedType.Undefined, ExpectedType.Specific(U0)) contains ex, (), TyperError.CantMatch(ex, U0, loc): TyperError))
          } yield Typed(ast.ValDef(name, tpe, typedVal): ast.BlockStatement, U0)

        case P.VarDef(name, tpe, value) => // TODO remove this ugly duplication
          for {
            c <- StateT.get[TyperResult, Ctxt]
            expectedType = tpe.map(t => ExpectedType.Specific(lookupType(t, c))).getOrElse(ExpectedType.Undefined)
            typedVal <- StateT.lift(solveType(value, expectedType, c))
            _ <- StateT.modify[TyperResult, Ctxt](_.withTerms(Map(name -> Ctxt.Mutable(typedVal.typ))))
            _ <- StateT.lift(Either.cond(Set(ExpectedType.Undefined, ExpectedType.Specific(U0)) contains ex, (), TyperError.CantMatch(ex, U0, loc): TyperError))
          } yield Typed(ast.VarDef(name, tpe, typedVal): ast.BlockStatement, U0)
      }
      (for {
        body <- stmnts.init.traverse(stmnt(_, ExpectedType.Undefined))
        last <- stmnt(stmnts.last, expected)
        t <- StateT.lift(unifyType(Typed(ast.Block(body :+ last, loc), last.typ), expected))
      } yield t).runA(ctxt)

    case P.Assign(l, Some(op), r, loc) =>
      solveType(P.Assign(l, None, P.InfixAp(op, l, r, loc), loc), expected, ctxt)

    case P.Assign(lval, None, rval, loc) =>
      for {
        ltyped <- solveLVal(lval, ctxt)
        rtyped <- solveType(rval, ExpectedType.Specific(ltyped.typ), ctxt)
        t <- unifyType(Typed(ast.Assign(ltyped, None, rtyped, loc), U0), expected)
      } yield t
  }

  private def solveLVal(e: P.Expression, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = e match {
    case P.Var(s, loc) => ctxt.terms(s) match {
      case Ctxt.Mutable(t) => Right(Typed(ast.Var(s, loc), t))
      case Ctxt.Immutable(_) => Left(TyperError.ImmutableAssign(s, loc))
    }
    case P.Select(s, member, loc) =>
      solveLVal(s, ctxt).flatMap {
        case src @ Typed(_, struct @ Struct(_, members)) =>
          members.find(_.name == member) map {
            case StructMember(_, typ) => Typed(ast.Select(src, member, loc), typ)
          } toRight TyperError.MemberNotFound(member, struct, loc)
      }
    case e => solveType(e, ExpectedType.Undefined, ctxt).flatMap {
      case lTyped @ Typed(ast.App(Typed(ptr, _: Ptr), _, _), _) => Right(lTyped)
      case lTyped @ Typed(ast.App(Typed(_, _: Arr), _, _), _) => Right(lTyped) // TODO arr ref too has to be an lval
      case e => Left(TyperError.NotAnLVal(e, e.expr.loc))
    }
  }

  private def cast(target: TypeName, src: Typed[ast.Expression], loc: Span, ctxt: Ctxt): Typed[ast.Expression] =
    (lookupType(target, ctxt), src.typ) match {
      case (a, b) if a == b => src
      case (t: Integral, s: Integral) =>
        val w = wider(t, s)
        if(w == t) widen(src, t)
        else trim(src, t)
      case (t: Integral, s: Type.F) =>
        Typed(ast.Convert(src, loc), t)
      case (t: Type.F, s: Integral) =>
        Typed(ast.Convert(src, loc), t)
      case (t @ Ptr(_), Ptr(_) | U64) => Typed(ast.Reinterpret(src, loc), t)
    }

  def lookupType(t: TypeName, ctxt: Ctxt): Type = t match {
    case TypeName.App(TypeName.Named(Symbol.Global(List("ptr"))), List(pointee)) => Type.Ptr(lookupType(pointee, ctxt))
    case TypeName.App(TypeName.Named(Symbol.Global(List("arr"))), List(member, TypeName.IntLiteral(size))) =>
      Type.Arr(lookupType(member, ctxt), size.toInt)
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
      else if(w == typ) Left(TyperError.CantMatch(expected, typ, t.expr.loc))
      else Right(widen(t, w))
    case (typ, ExpectedType.Numeric(_)) => Left(TyperError.CantMatch(expected, typ, t.expr.loc))

    case (f: Fun, ExpectedType.Appliable) => Right(t)
    case (p: Ptr, ExpectedType.Appliable) => Right(t)
    case (a: Arr, ExpectedType.Appliable) => Right(t)
    case (_, ExpectedType.Appliable) => Left(TyperError.CantMatch(expected, t.typ, t.expr.loc))

    case (typ, ExpectedType.Specific(U0))
      if typ != U0 => Right(Typed(ast.Ignore(t, t.expr.loc), U0))

    case (typ, ExpectedType.Specific(u)) =>
      if(typ == u) Right(t) else Left(TyperError.CantMatch(expected, typ, t.expr.loc))
    case (_, ExpectedType.Undefined) => Right(t)
  }

  private def wider(a: Type, b: Type): Type = (a, b) match {
    case (a, b) if a == b => a
    case (a: Integral, b: Integral) =>
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
    case (F64, _: F) | (_: F, F64) => F64
  }

  private def widen(expr: Typed[ast.Expression], t: Type): Typed[ast.Expression] = expr match {
    case Typed(l: ast.IntLit, _) => Typed(l, t)
    case Typed(l: ast.FloatLit, _) => Typed(l, t)
    case Typed(ast.CharLit(c, loc), _) => Typed(ast.IntLit(c.toLong, loc), t)
    case Typed(ast.Widen(e, _), _) => widen(e, t)
    case _ => Typed(ast.Widen(expr, expr.expr.loc), t)
  }

  private def trim(expr: Typed[ast.Expression], t: Type): Typed[ast.Expression] = expr match {
    case Typed(l: ast.IntLit, _) => Typed(l, t)
    case Typed(ast.Trim(e, _), _) => trim(e, t)
    case _ => Typed(ast.Trim(expr, expr.expr.loc), t)
  }

  private def bitwidth(t: Type): Int = t match {
    case U1 => 1
    case U8 | I8 => 8
    case U16 | I16 => 16
    case U32 | I32 => 32
    case U64 | I64 => 64
  }

  private def signed(t: Type): Type = t match {
    case t: F => t
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
