package oxidation
package analyze

import parse.{Span, ast => P}
import Type._
import cats._
import cats.data._
import cats.implicits._

import Validated.{valid, invalidNel}

object Typer {

  type TyperResult[A] = ValidatedNel[TyperError, A]

  def solveType(expression: P.Expression, expected: ExpectedType, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = expression match {
    case P.IntLit(i, loc) =>
      expected match {
        case ExpectedType.Numeric(None) | ExpectedType.Undefined | ExpectedType.Value | ExpectedType.Specific(Type.Implicit) =>
          valid(Typed(ast.IntLit(i, loc), I32))
        case ExpectedType.Numeric(Some(t: Integral)) => valid(Typed(ast.IntLit(i, loc), t))
        case ExpectedType.Specific(t: Integral) => valid(Typed(ast.IntLit(i, loc), t))
        case _ => unifyType(Typed(ast.IntLit(i, loc), I32), expected)
      }

    case P.FloatLit(f, loc) =>
      expected match {
        case ExpectedType.Numeric(None) | ExpectedType.Undefined | ExpectedType.Value | ExpectedType.Specific(Type.Implicit) =>
          valid(Typed(ast.FloatLit(f, loc), F64))
        case ExpectedType.Numeric(Some(t: F)) => valid(Typed(ast.FloatLit(f, loc), t))
        case ExpectedType.Specific(t: F) => valid(Typed(ast.FloatLit(f, loc), t))
        case _ => unifyType(Typed(ast.FloatLit(f, loc), F64), expected)
      }

    case P.BoolLit(b, loc) => unifyType(Typed(ast.BoolLit(b, loc), U1), expected)

    case P.CharLit(c, loc) => unifyType(Typed(ast.CharLit(c, loc), U8), expected)

    case P.StringLit(s, loc) => unifyType(Typed(ast.StringLit(s, loc), BuiltinSymbols.StrType), expected)

    case P.UnitLit(loc) => unifyType(Typed(ast.UnitLit(loc), U0), expected)

    case P.Extern(loc) => expected match {
      case ExpectedType.Specific(t) => valid(Typed(ast.Extern(loc), t))
      case _ => invalidNel(TyperError.ExternNoExplicitType(loc))
    }

    case P.App(P.TypeApp(P.Var(Symbol.Global(List("cast")), _), List(target), _), List(src), loc) =>
      solveType(src, ExpectedType.Value, ctxt).map(cast(target, _, loc, ctxt))

    case P.TypeApp(P.Var(Symbol.Global(List("stackalloc")), _), List(pointee), loc) =>
      unifyType(Typed(ast.Stackalloc(lookupType(pointee, ctxt), loc), Ptr(lookupType(pointee, ctxt))), expected)

    case P.App(P.TypeApp(P.Var(Symbol.Global(List("arr")), _), membertn :: typeTail, _), termParams, loc) =>
      Validated.fromEither(for {
        size <- typeTail.headOption traverse {
          case TypeName.IntLiteral(i) => Right(i.toInt)
          case tn => Left(NonEmptyList.of(TyperError.NotASingletonType(tn)))
        }
        _ <- size match {
          case Some(s) => Either.cond(termParams.size == s || termParams.size == 1, (), NonEmptyList.of(TyperError.WrongNumberOfArguments(s, termParams.size, loc)))
          case None => Right(())
        }
        memberType = lookupType(membertn, ctxt)
        typedParams <- termParams.traverse(solveType(_, ExpectedType.Specific(memberType), ctxt)).toEither
      } yield Typed(ast.ArrLit(typedParams, loc), Arr(memberType, size getOrElse typedParams.size)))


    case P.App(P.Var(sqrt @ Symbol.Global(List("sqrt")), sqrtloc), List(param), loc) =>
      Validated.fromEither(for {
        typedParam <- solveType(param, ExpectedType.Numeric(None), ctxt).toEither
        pt = typedParam.typ
        _ <- Either.cond(pt.isInstanceOf[Type.F], (), NonEmptyList.of(TyperError.CantMatch(ExpectedType.Specific(Type.F64), pt, param.loc)))
      } yield Typed(ast.App(Typed(ast.Var(sqrt, sqrtloc), Type.Fun(List(pt), pt)), List(typedParam), loc), pt))

    case P.StructLit(name, typeParams, members, loc) =>
      def solveMembers(expectedType: String => ExpectedType): TyperResult[List[(String, Typed[ast.Expression])]] =
        members.traverse {
          case (name, expr) =>
            solveType(expr, expectedType(name), ctxt).map(name -> _)
        }

      def checkMemberSet(expected: Set[String], found: Set[String]): TyperResult[Unit] =
        if(expected == found) valid(()) else invalidNel(TyperError.WrongStructMembers(expected, found, loc))

      def solveStruct(typ: Type): TyperResult[Typed[ast.Expression]] = typ.repr match {
        case struct: Type.Struct =>
          val correctMemberSet = checkMemberSet(members.map(_._1).toSet, struct.members.map(_.name).toSet)
          val expectedTypeFun = struct.members.map(m => m.name -> m.typ).toMap
            .andThen(ExpectedType.Specific)
            .lift.andThen(_ getOrElse ExpectedType.Undefined)

          correctMemberSet *>
            solveMembers(expectedTypeFun).map(typedMembers => Typed(ast.StructLit(struct.symbol, None, typedMembers, loc), typ))
        case t => invalidNel(TyperError.NotAStruct(t, loc))
      }

      ctxt.types.get(name).map {
        case struct: Type.Struct =>
          val noParams = typeParams match {
            case None => valid(())
            case Some(_) => invalidNel(TyperError.DoesNotAcceptParams(struct, loc): TyperError)
          }
          noParams *> solveStruct(struct)

        case cons @ Type.TypeLambda(_, _, struct: Type.Struct) =>
          typeParams match {
            case Some(tps) =>
              val paramCountMatch =
                if(tps.size == cons.paramNames.size) valid(())
                else invalidNel(TyperError.WrongNumberOfTypeParams(cons, tps.size, loc))
              paramCountMatch.andThen(_ => solveStruct(Type.App(cons, (tps.map(lookupType(_, ctxt))))))
            case None =>
              val expectedTypeFun = struct.members.map(m => m.name -> m.typ.implyVars(cons.paramNames.toSet)).toMap
                .andThen(ExpectedType.Specific)
                .lift.andThen(_ getOrElse ExpectedType.Undefined)
              solveMembers(expectedTypeFun).andThen { typedMembers =>
                typedMembers.traverse {
                  case (n, v) => unifyShape(struct.members.find(_.name == n).get.typ, v.typ, loc)
                }.andThen { constrs =>
                  val flat = constrs.flatten
                  // TODO error messages here are abysmal
                  flat.groupBy(_._1).mapValues(_.map(_._2)).toVector.traverse {
                    case (n, t :: ts) =>
                      ts.traverse_(x => if(x.repr == t.repr) valid(())
                        else invalidNel(TyperError.CantMatch(ExpectedType.Specific(t), x, loc))) as (n -> t)
                  }.map { substs =>
                    val substMap = substs.toMap
                    val typ = Type.App(cons, cons.paramNames.map(substMap))
                    Typed(ast.StructLit(typ.symbol, None, typedMembers, loc), typ)
                  }
                }
              }
              
          }
        
      }.getOrElse {
        ctxt.terms(name).typ match {
          case Type.EnumConstructor(enum, variant) =>
            val correctMemberSet = checkMemberSet(members.map(_._1).toSet, variant.members.map(_.name).toSet)

            val expectedTypeFun = variant.members.map(m => m.name -> m.typ).toMap
              .andThen(ExpectedType.Specific)
              .lift.andThen(_ getOrElse ExpectedType.Undefined)

            correctMemberSet *>
              solveMembers(expectedTypeFun).map(typedMembers => Typed(ast.EnumLit(variant.name.name, typedMembers, loc), enum))
        }
      }

    case P.PrefixAp(PrefixOp.Inv, right, loc) =>
      solveType(right, ExpectedType.Numeric(None), ctxt)
        .andThen(rtyped => unifyType(Typed(ast.PrefixAp(PrefixOp.Inv, rtyped, loc), rtyped.typ), expected))

    case P.PrefixAp(PrefixOp.Neg, right, loc) =>
      solveType(right, ExpectedType.Numeric(None), ctxt)
        .andThen(rtyped => unifyType(Typed(ast.PrefixAp(PrefixOp.Neg, rtyped, loc), signed(rtyped.typ)), expected))

    case P.PrefixAp(PrefixOp.Not, right, loc) =>
      solveType(right, ExpectedType.Specific(U1), ctxt)
        .andThen(rtyped => unifyType(Typed(ast.PrefixAp(PrefixOp.Not, rtyped, loc), rtyped.typ), expected))

    case P.InfixAp(op, left, right, loc) =>
      op match {
        case InfixOp.Add | InfixOp.Sub | InfixOp.Mul | InfixOp.Div |
             InfixOp.Mod | InfixOp.BitAnd | InfixOp.BitOr | InfixOp.Shl | InfixOp.Shr =>
          val lexpected = expected match {
            case ExpectedType.Numeric(Some(_)) => expected
            case ExpectedType.Specific(_) => expected
            case _ => ExpectedType.Numeric(None)
          }
          Validated.fromEither(for {
            ltyped <- solveType(left, lexpected, ctxt).toEither
            rtyped <- solveType(right, ExpectedType.Numeric(Some(ltyped.typ)), ctxt).toEither
            lunified <- unifyType(ltyped, ExpectedType.Specific(rtyped.typ)).toEither
            t <- unifyType(Typed(ast.InfixAp(op, lunified, rtyped, loc), rtyped.typ), expected).toEither
          } yield t)

        case InfixOp.Eq | InfixOp.Neq =>
          Validated.fromEither(for {
            ltyped <- solveType(left, ExpectedType.Value, ctxt).toEither
            rightExpected = ltyped.typ match {
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => ExpectedType.Numeric(Some(ltyped.typ))
              case t => ExpectedType.Specific(t)
            }
            rtyped <- solveType(right, rightExpected, ctxt).toEither
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped, loc): ast.Expression, U1), expected).toEither
          } yield t)

        case InfixOp.Geq | InfixOp.Gt | InfixOp.Lt | InfixOp.Leq =>
          Validated.fromEither(for {
            ltyped <- solveType(left, ExpectedType.Numeric(None), ctxt).toEither
            rtyped <- solveType(right, ExpectedType.Numeric(Some(ltyped.typ)), ctxt).toEither
            widerType = wider(ltyped.typ, rtyped.typ)
            lwidened = if (ltyped.typ == widerType) ltyped else Typed(ast.Widen(ltyped, ltyped.expr.loc), widerType)
            rwidened = if (rtyped.typ == widerType) rtyped else Typed(ast.Widen(rtyped, rtyped.expr.loc), widerType)
            t <- unifyType(Typed(ast.InfixAp(op, lwidened, rwidened, loc): ast.Expression, U1), expected).toEither
          } yield t)

        case InfixOp.And | InfixOp.Or =>
          (solveType(left, ExpectedType.Specific(U1), ctxt), solveType(right, ExpectedType.Specific(U1), ctxt))
            .map2((_, _))
            .andThen { case (l, r) => unifyType(Typed(ast.InfixAp(op, l, r, loc), U1), expected) }

        case InfixOp.Xor =>
          Validated.fromEither(for {
            ltyped <- solveType(left, ExpectedType.Value, ctxt).toEither
            rightExpected <- ltyped.typ match {
              case U1 => Right(ExpectedType.Specific(U1))
              case U8 | U16 | U32 | U64 | I8 | I16 | I32 | I64 => Right(ExpectedType.Numeric(Some(ltyped.typ)))
              case t => Left(NonEmptyList.of(TyperError.CantMatch(ExpectedType.Undefined, t, left.loc)))
            }
            rtyped <- solveType(right, rightExpected, ctxt).toEither
            t <- unifyType(Typed(ast.InfixAp(op, ltyped, rtyped, loc): ast.Expression, U1), expected).toEither // TODO support xor on integers
          } yield t)
      }

    case P.Method(obj, meth, loc) =>
      solveType(P.App(meth, List(obj), loc), expected, ctxt)

    case P.App(P.Method(target, method, methLoc), params, appLoc) =>
      solveType(P.App(method, target :: params, appLoc), expected, ctxt)

    case P.TypeApp(P.Method(target, method, methLoc), params, appLoc) =>
      solveType(P.App(P.TypeApp(method, params, appLoc), List(target), appLoc), expected, ctxt)

    case P.App(fn, pars, loc) =>
      solveType(fn, ExpectedType.Appliable, ctxt) andThen {
        case typedFun @ ReprTyped(_, Fun(paramTypes, retType)) =>
          val typedParams = (pars zip paramTypes).traverse {
            case (e, t) => solveType(e, ExpectedType.Specific(t), ctxt)
          }
          val paramCount =
            if(paramTypes.size == pars.size) valid(())
            else invalidNel(TyperError.WrongNumberOfArguments(paramTypes.size, pars.size, loc))
          (typedParams <* paramCount)
            .andThen(ps => unifyType(Typed(ast.App(typedFun, ps, loc), retType), expected))

        case typedFun @ ReprTyped(_, FunPtr(paramTypes, retType)) =>
          val typedParams = (pars zip paramTypes).traverse {
            case (e, t) => solveType(e, ExpectedType.Specific(t), ctxt)
          }
          val paramCount =
            if(paramTypes.size == pars.size) valid(())
            else invalidNel(TyperError.WrongNumberOfArguments(paramTypes.size, pars.size, loc))
          (typedParams <* paramCount)
            .andThen(ps => unifyType(Typed(ast.App(typedFun, ps, loc), retType), expected))


        case typedPtr @ ReprTyped(_, Ptr(pointee)) =>
          val typedParams = pars match {
            case Nil => valid(Nil)
            case off :: Nil => solveType(off, ExpectedType.Specific(I64), ctxt).map(List(_))
            case ps => invalidNel(TyperError.TooManyParamsForPointerDereference(ps.size, loc))
          }
          typedParams.andThen(ps => unifyType(Typed(ast.App(typedPtr, ps, loc), pointee), expected))

        case typedArr @ ReprTyped(_, Arr(memberType, _)) =>
          val typedParams = pars match {
            case index :: Nil => solveType(index, ExpectedType.Specific(I64), ctxt).map(List(_))
            case _ => invalidNel(TyperError.WrongNumberOfArguments(1, pars.size, loc))
          }
          typedParams.andThen(ps => unifyType(Typed(ast.App(typedArr, ps, loc), memberType), expected))
      }


    case P.Var(name, loc) =>
      ctxt.terms.get(name).map(_.typ)
        .toValidNel(TyperError.SymbolNotFound(name, loc))
        .andThen { t =>
            unifyType(Typed(ast.Var(name, loc), t), expected)
        }

    case P.Select(expr, member, loc) =>
      Validated.fromEither(for {
        exprTyped <- solveType(expr, ExpectedType.Value, ctxt).toEither
        memberType <- exprTyped.typ.repr match {
          case t @ Struct(_, ms) =>
            ms.find(_.name == member).map(_.typ).toRight(NonEmptyList.of(TyperError.MemberNotFound(member, t, loc)))
          case Ptr(s @ Struct(_, ms)) =>
            ms.find(_.name == member).map(x => Ptr(x.typ)).toRight(NonEmptyList.of(TyperError.MemberNotFound(member, s, loc)))
          case t =>
            Left(NonEmptyList.of(TyperError.MemberNotFound(member, t, loc)))
        }
        t <- unifyType(Typed(ast.Select(exprTyped, member, loc), memberType), expected).toEither
      } yield t)

    case P.If(cond, pos, None, loc) =>
      ( solveType(cond, ExpectedType.Specific(U1), ctxt)
      , solveType(pos, ExpectedType.Value, ctxt))
        .map2((_, _))
        .andThen {
          case (c, p) => unifyType(Typed(ast.If(c, p, None, loc), U0), expected)
        }

    case P.If(cond, pos, Some(neg), loc) =>
      val condTyped = solveType(cond, ExpectedType.Specific(U1), ctxt)
      val bodies = Validated.fromEither(for {
        posTyped <- solveType(pos, expected, ctxt).toEither
        negExpected = (expected, posTyped.typ) match {
          case (ExpectedType.Specific(t), _) => expected
          case (ExpectedType.Numeric(t), _) => ExpectedType.Numeric(t)
          case (ExpectedType.Appliable, t) => ExpectedType.Specific(t)
          case (ExpectedType.Undefined | ExpectedType.Value | ExpectedType.Specific(Type.Implicit), t) => ExpectedType.Specific(t)
        }
        negTyped <- solveType(neg, negExpected, ctxt).toEither
      } yield (posTyped, negTyped))

      (condTyped, bodies)
        .map2 {
          case (condTyped, (posTyped, negTyped)) =>
            val t = (posTyped.typ, negTyped.typ) match {
              case (a, b) if a == b => a
              case (a, b) => wider(a, b) // TODO case this for numeric types only
            }
            Typed(ast.If(condTyped, posTyped, Some(negTyped), loc), t)
        }.andThen(unifyType(_, expected))

    case P.While(cond, body, loc) =>
      ( solveType(cond, ExpectedType.Specific(U1), ctxt)
      , solveType(body, ExpectedType.Undefined, ctxt))
        .map2((_, _))
        .andThen { case (c, b) => unifyType(Typed(ast.While(c, b, loc), U0), expected) }

    case P.Match(matchee, cases, loc) =>
      solveType(matchee, ExpectedType.Value, ctxt)
        .andThen { typedMatchee =>
          cases.traverse(solveMatchCase(typedMatchee.typ, expected, ctxt))
            .andThen { typedCases =>
            val unhandled = typedCases.foldLeft(MatchSet.Any(typedMatchee.typ) : MatchSet) {
              case (set, ast.MatchCase(Typed(pattern, _), None, _)) => set - pattern
              case (set, ast.MatchCase(_, Some(_), _)) => set
            }
            val resultType = typedCases.map(_.body.typ).reduceLeft(commonType)
            val unifiedCases = typedCases.traverse {
              case ast.MatchCase(pat, guard, body) =>
                unifyType(body, ExpectedType.Specific(resultType))
                  .map(ast.MatchCase(pat, guard, _))
            }
            unifiedCases
              .andThen(cs => unifyType(Typed(ast.Match(typedMatchee, cs, loc), resultType), expected)) <*
              (if(unhandled.isEmpty) valid(()) else invalidNel(TyperError.NonexhaustivePatternMatch(unhandled, loc)))
          }
        }

    case P.Block(Vector(), loc) =>
      unifyType(Typed(ast.Block(Vector.empty, loc), U0), expected)

    case P.Block(stmnts, loc) =>
      type S[A] = State[Ctxt, A]
      val S: MonadState[S, Ctxt] = implicitly
      def stmnt(s: P.BlockStatement, ex: ExpectedType): S[TyperResult[Typed[ast.BlockStatement]]] = s match {
        case e: P.Expression =>
          S.get.map(solveType(e, ex, _))

        case P.ValDef(name, tpe, value) =>
          for {
            c <- S.get
            declaredType = tpe.map(lookupType(_, c))
            expectedType = declaredType.map(ExpectedType.Specific).getOrElse(ExpectedType.Value)
            expectedMatch =
              if(Set(ExpectedType.Value, ExpectedType.Undefined, ExpectedType.Specific(U0), ExpectedType.Specific(Type.Implicit)) contains ex)
                valid(())
              else invalidNel(TyperError.CantMatch(ex, U0, loc): TyperError)
            typedVal = solveType(value, expectedType, c) <* expectedMatch
            inferredType = declaredType.orElse(typedVal.toOption.map(_.typ))
            // When no explicit type annotation is given and typechecking fails for the right side the symbol is left undefined
            // The typechecking of the whole block will fail anyway, and this will give us an opportunity to find more errors further
            // in the block.
            _ <- inferredType.traverse(t => S.modify(_.withTerms(Map(name -> Ctxt.Immutable(t)))))
          } yield typedVal.map(tv => Typed(ast.ValDef(name, tpe, tv): ast.BlockStatement, U0))

        case P.VarDef(name, tpe, value) => // TODO remove this ugly duplication
          for {
            c <- S.get
            declaredType = tpe.map(lookupType(_, c))
            expectedType = declaredType.map(ExpectedType.Specific).getOrElse(ExpectedType.Value)
            expectedMatch =
              if(Set(ExpectedType.Value, ExpectedType.Undefined, ExpectedType.Specific(U0), ExpectedType.Specific(Type.Implicit)) contains ex)
                valid(())
              else invalidNel(TyperError.CantMatch(ex, U0, loc): TyperError)
            typedVal = solveType(value, expectedType, c) <* expectedMatch
            inferredType = declaredType.orElse(typedVal.toOption.map(_.typ))
            _ <- inferredType.traverse(t => S.modify(_.withTerms(Map(name -> Ctxt.Mutable(t)))))
          } yield typedVal.map(tv => Typed(ast.VarDef(name, tpe, tv): ast.BlockStatement, U0))
      }
      val validated = (for {
        body <- stmnts.init.traverse(stmnt(_, ExpectedType.Value))
        last <- stmnt(stmnts.last, expected)
      } yield body :+ last).runA(ctxt).value
      validated.sequence.andThen(stmnts => unifyType(Typed(ast.Block(stmnts, loc), stmnts.last.typ), expected))

    case P.Assign(l, Some(op), r, loc) =>
      solveType(P.Assign(l, None, P.InfixAp(op, l, r, loc), loc), expected, ctxt)

    case P.Assign(lval, None, rval, loc) =>
      Validated.fromEither(for {
        ltyped <- solveLVal(lval, ctxt).toEither
        rtyped <- solveType(rval, ExpectedType.Specific(ltyped.typ), ctxt).toEither
        t <- unifyType(Typed(ast.Assign(ltyped, None, rtyped, loc), U0), expected).toEither
      } yield t)
  }

  private def unifyShape(left: Type, right: Type, loc: Span): TyperResult[List[(String, Type)]] = (left, right) match {
    case (Type.Var(a), r) => valid(List((a, r)))
    case (Type.App(a, ap), Type.App(b, bp)) =>
      val paramCountMatch =
        if(ap.size == bp.size) valid(())
        else invalidNel(TyperError.WrongNumberOfArguments(ap.size, bp.size, loc): TyperError)
      paramCountMatch *>
        (unifyShape(a, b, loc), (ap zip bp).traverse { case (a, b) => unifyShape(a, b, loc) })
        .map2(_ ++ _.flatten)
    case (a, b) if a.repr == b.repr => valid(Nil)
    case _ => invalidNel(TyperError.CantMatch(ExpectedType.Specific(left), right, loc))
  }

  private def solveMatchCase(matcheeType: Type, expectedType: ExpectedType, ctxt: Ctxt)
                            (cas: P.MatchCase): TyperResult[ast.MatchCase] = {
    def findBindings(pattern: Typed[ast.Pattern]): TyperResult[List[(Symbol, Ctxt.Term)]] = pattern match {
      case Typed(ast.Pattern.Var(n, _), t) => valid(List(n -> Ctxt.Immutable(t)))
      case Typed(ast.Pattern.Alias(n, p, _), t) =>
        findBindings(p).map((n -> Ctxt.Immutable(t)) :: _)
      case Typed(ast.Pattern.Struct(_, ms, _, _), _) => ms.traverse(m => findBindings(m._2)).map(_.flatten)
      case Typed(ast.Pattern.Enum(_, ms, _, _), _) => ms.traverse(m => findBindings(m._2)).map(_.flatten)
      case Typed(ast.Pattern.Or(left, right, loc), _) =>
        (findBindings(left), findBindings(right))
          .map2((_, _))
          .andThen { case (l, r) =>
            if(l.toSet == r.toSet) valid(l)
            else invalidNel(TyperError.AlternativePatternBindingsMismatch(l.toSet, r.toSet, loc))
          }

      case Typed(ast.Pattern.Ignore(_) | ast.Pattern.IntLit(_, _) | ast.Pattern.FloatLit(_, _) | ast.Pattern.BoolLit(_, _)
           | ast.Pattern.CharLit(_, _) | ast.Pattern.Pin(_, _), _) => valid(Nil)
    }
    solvePattern(cas.pattern, matcheeType, ctxt).andThen { typedPattern =>
      findBindings(typedPattern).andThen { bs =>
        val bodyCtxt = ctxt.withTerms(bs.toMap)
        ( cas.guard.traverse(solveType(_, ExpectedType.Specific(U1), bodyCtxt)),
          solveType(cas.body, expectedType, bodyCtxt))
          .map2(ast.MatchCase(typedPattern, _, _))
      }
    }
  }

  private def solvePattern(pattern: P.Pattern, matcheeType: Type, ctxt: Ctxt): TyperResult[Typed[ast.Pattern]] = pattern match {
    case P.Pattern.Ignore(loc) => valid(Typed(ast.Pattern.Ignore(loc), matcheeType))
    case P.Pattern.Var(name: Symbol.Local, loc) => valid(Typed(ast.Pattern.Var(name, loc), matcheeType))
    case P.Pattern.Var(name: Symbol.Global, loc) => matcheeType match {
      case _: Type.Enum => solvePattern(P.Pattern.Struct(Some(name), Nil, ignoreExtra = false, loc), matcheeType, ctxt)
      case _ => solvePattern(P.Pattern.Pin(P.Var(name, loc), loc), matcheeType, ctxt)
    }
    case P.Pattern.Alias(name, pattern, loc) =>
      solvePattern(pattern, matcheeType, ctxt)
        .map(p => Typed(ast.Pattern.Alias(name, p, loc), matcheeType))
    case P.Pattern.Pin(subexp, loc) =>
      solveType(subexp, ExpectedType.Specific(matcheeType), ctxt)
        .map(s => Typed(ast.Pattern.Pin(s, loc), matcheeType))

    case P.Pattern.IntLit(value, loc) => matcheeType match {
      case t: Integral => valid(Typed(ast.Pattern.IntLit(value, loc), t))
      case t => invalidNel(TyperError.CantMatch(ExpectedType.Numeric(Some(I32)), t, loc))
    }
    case P.Pattern.CharLit(value, loc) => matcheeType match {
      case t: Integral => valid(Typed(ast.Pattern.IntLit(value, loc), t))
      case t => invalidNel(TyperError.CantMatch(ExpectedType.Numeric(Some(U8)), t, loc))
    }
    case P.Pattern.FloatLit(value, loc) => matcheeType match {
      case t: F => valid(Typed(ast.Pattern.FloatLit(value, loc), t))
      case t => invalidNel(TyperError.CantMatch(ExpectedType.Numeric(Some(F64)), t, loc))
    }
    case P.Pattern.BoolLit(value, loc) => matcheeType match {
      case U1 => valid(Typed(ast.Pattern.BoolLit(value, loc), U1))
      case t => invalidNel(TyperError.CantMatch(ExpectedType.Specific(U1), t, loc))
    }
    case P.Pattern.Struct(typ, memberPatterns, ignoreExtra, loc) => matcheeType match {
      case Type.Struct(name, matcheeMembers) =>
        val typedMembers = memberPatterns.traverse {
          case (name, pattern) =>
            matcheeMembers.find(_.name == name).toValidNel(TyperError.MemberNotFound(name, matcheeType, pattern.loc))
              .andThen(member => solvePattern(pattern, member.typ, ctxt).map(name -> _))
        }
        lazy val matcheeMemberNames = matcheeMembers.map(_.name).toSet
        lazy val patternMemberNames = memberPatterns.map(_._1).toSet
        val allMembersHandled =
          if(ignoreExtra || matcheeMemberNames == patternMemberNames)
            valid(())
          else invalidNel(TyperError.WrongStructMembers(matcheeMemberNames, patternMemberNames, loc))
        val correctExplicitType = typ.flatMap { explicitName =>
          val explicitType = lookupType(TypeName.Named(explicitName), ctxt)
          if(explicitType == matcheeType) None
          else Some(TyperError.CantMatch(ExpectedType.Specific(matcheeType), explicitType, loc))
        }.toInvalidNel(())

        typedMembers.map(ms => Typed(ast.Pattern.Struct(typ, ms, ignoreExtra, loc), matcheeType)) <*
        allMembersHandled <* correctExplicitType

      case enumType @ Type.Enum(name, matcheeVariants) =>
        typ.toValidNel(TyperError.NoVariantSpecified(loc))
          .andThen(variantName => matcheeVariants.find(_.name == variantName)
            .toValidNel(TyperError.VariantNotFound(enumType, variantName, loc)))
          .andThen { variant =>
            val typedMembers = memberPatterns.traverse {
              case (name, pattern) =>
                variant.members.find(_.name == name).toValidNel(TyperError.MemberNotFound(name, matcheeType, pattern.loc))
                  .andThen(member => solvePattern(pattern, member.typ, ctxt).map(name -> _))
            }
            lazy val matcheeMemberNames = variant.members.map(_.name).toSet
            lazy val patternMemberNames = memberPatterns.map(_._1).toSet
            val allMembersHandled =
              if(ignoreExtra || matcheeMemberNames == patternMemberNames)
                valid(())
              else invalidNel(TyperError.WrongStructMembers(matcheeMemberNames, patternMemberNames, loc))

            typedMembers.map(ms => Typed(ast.Pattern.Enum(variant.name, ms, ignoreExtra, loc), matcheeType))
          }
        
    }
    case P.Pattern.Or(left, right, loc) =>
      (solvePattern(left, matcheeType, ctxt), solvePattern(right, matcheeType, ctxt))
        .map2((a, b) => Typed(ast.Pattern.Or(a, b, loc), matcheeType))
  }

  private def solveLVal(e: P.Expression, ctxt: Ctxt): TyperResult[Typed[ast.Expression]] = e match {
    case P.Var(s, loc) => ctxt.terms(s) match {
      case Ctxt.Mutable(t) => valid(Typed(ast.Var(s, loc), t))
      case Ctxt.Immutable(_) => invalidNel(TyperError.ImmutableAssign(s, loc))
    }
    case P.Select(s, member, loc) =>
      solveLVal(s, ctxt).andThen {
        case src @ Typed(_, struct @ Struct(_, members)) =>
          members.find(_.name == member) map {
            case StructMember(_, typ) => Typed(ast.Select(src, member, loc), typ)
          } toValidNel TyperError.MemberNotFound(member, struct, loc)
      }
    case e => solveType(e, ExpectedType.Undefined, ctxt).andThen {
      case lTyped @ Typed(ast.App(Typed(ptr, _: Ptr), _, _), _) => valid(lTyped)
      case lTyped @ Typed(ast.App(Typed(_, _: Arr), _, _), _) => valid(lTyped) // TODO arr ref too has to be an lval
      case e => invalidNel(TyperError.NotAnLVal(e, e.expr.loc))
    }
  }

  private def cast(target: TypeName, src: Typed[ast.Expression], loc: Span, ctxt: Ctxt): Typed[ast.Expression] = {
    val tarType = lookupType(target, ctxt)
    (tarType.repr, src.typ.repr) match {
      case (a, b) if a == b => src
      case (t: Integral, s: Integral) =>
        val w = wider(t, s)
        if(w == t) widen(src, t)
        else trim(src, t)
      case (t: Integral, s: Type.F) =>
        Typed(ast.Convert(src, loc), t)
      case (t: Type.F, s: Integral) =>
        Typed(ast.Convert(src, loc), t)
      case (Ptr(_), Ptr(_) | U64) => Typed(ast.Reinterpret(src, loc), tarType)
    }
  }

  def lookupType(t: TypeName, ctxt: Ctxt): Type = t match {
    case TypeName.App(TypeName.Named(Symbol.Global(List("arr"))), List(member, TypeName.IntLiteral(size))) =>
      Type.Arr(lookupType(member, ctxt), size.toInt)
    case TypeName.App(TypeName.Named(Symbol.Global(List("funptr"))), List(TypeName.Fun(params, ret))) =>
      Type.FunPtr(params.map(lookupType(_, ctxt)), lookupType(ret, ctxt))
    case TypeName.App(l, ps) => lookupType(l, ctxt) match {
      case cons: Type.TypeLambda => Type.App(cons, ps.map(lookupType(_, ctxt)))
    }
    case TypeName.Named(s) => ctxt.types(s)
  }

  def solveTermDef(d: P.TermDef, ctxt: Ctxt): TyperResult[ast.TermDef] = {
    val expectedType = d.typ.map(ExpectedType.Specific compose (lookupType(_, ctxt)))
      .getOrElse(ExpectedType.Value)
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

  private def unifyType(t: Typed[ast.Expression], expected: ExpectedType): TyperResult[Typed[ast.Expression]] = (t.typ.repr, expected) match {
    case (_: Num, ExpectedType.Numeric(None)) => valid(t)
    case (_: Num, ExpectedType.Numeric(Some(lb))) =>
      val w = wider(t.typ, lb)
      valid(if(w == t.typ) t else widen(t, w))
    case (typ: Num, ExpectedType.Specific(exp: Num)) =>
      val w = wider(typ, exp)
      if(typ == exp) valid(t)
      else if(w == typ) invalidNel(TyperError.CantMatch(expected, typ, t.expr.loc))
      else valid(widen(t, w))
    case (typ, ExpectedType.Numeric(_)) => invalidNel(TyperError.CantMatch(expected, typ, t.expr.loc))

    case (_: Fun | _: FunPtr | _: Ptr | _: Arr, ExpectedType.Appliable) => valid(t)
    case (_, ExpectedType.Appliable) => invalidNel(TyperError.CantMatch(expected, t.typ, t.expr.loc))

    case (typ, ExpectedType.Specific(U0))
      if typ != U0 => valid(Typed(ast.Ignore(t, t.expr.loc), U0))

    case (EnumConstructor(enum, EnumVariant(name, Nil)), ExpectedType.Specific(_: Enum) | ExpectedType.Value) =>
      unifyType(Typed(ast.EnumLit(name.name, Nil, t.expr.loc), enum), expected)

    case (f: Fun, ExpectedType.Value | ExpectedType.Specific(_: FunPtr)) =>
      unifyType(Typed(ast.Convert(t, t.expr.loc), f.ptr), expected)

    case (_: ValueType, ExpectedType.Value) => valid(t)

    case (typ, ExpectedType.Specific(u)) =>
      if(u moreGeneralThan typ) valid(t) else invalidNel(TyperError.CantMatch(expected, typ, t.expr.loc))
    case (_, ExpectedType.Undefined | ExpectedType.Specific(Type.Implicit)) => valid(t)
  }

  private[analyze] def commonType(a: Type, b: Type): Type = (a, b) match {
    case (a, b) if a == b => a
    case (a: Num, b: Num) => wider(a, b)
    case _ => U0
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
