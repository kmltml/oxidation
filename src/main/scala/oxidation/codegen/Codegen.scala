package oxidation
package codegen

import analyze.{BuiltinSymbols, Typed, ast}
import ir._
import cats._
import cats.data._
import cats.implicits._

object Codegen {

  final case class Log(insts: Vector[Inst], consts: Vector[ConstantPoolEntry] = Vector.empty)

  implicit val logMonoid: Monoid[Log] = new Monoid[Log] {
    override def empty: Log = Log(Vector.empty, Vector.empty)

    override def combine(x: Log, y: Log): Log = Log(x.insts |+| y.insts, x.consts |+| y.consts)
  }

  type Res[A] = WriterT[State[CodegenState, ?], Log, A]
  private val Res = MonadWriter[Res, Log]

  def instructions(insts: Inst*): Res[Unit] = Res.tell(Log(insts.toVector))
  def constants(consts: ConstantPoolEntry*): Res[Unit] = Res.tell(Log(Vector.empty, consts.toVector))

  object CodegenReg extends RegisterNamespace {
    override def prefix: String = "r"
  }

  def register(index: Int, typ: ir.Type): ir.Register = ir.Register(CodegenReg, index, typ)

  def compileExpr(expr: Typed[ast.Expression]): Res[Val] = expr match {
    case Typed(ast.IntLit(i, _), typ) => Res.pure(Val.I(i, translateType(typ)))
    case Typed(ast.FloatLit(bd, _), analyze.Type.F32) =>
      Res.pure(Val.F32(bd.toFloat))
    case Typed(ast.FloatLit(bd, _), analyze.Type.F64) =>
      Res.pure(Val.F64(bd.toDouble))

    case Typed(ast.BoolLit(b, _), typ) =>
      val i = b match {
        case true => 1
        case false => 0
      }
      Res.pure(Val.I(i, translateType(typ)))
    case Typed(ast.CharLit(c, _), typ) => Res.pure(Val.I(c.toInt, translateType(typ)))
    case Typed(ast.StructLit(_, members, _), analyze.Type.Struct(_, memberTypes)) =>
      for {
        memberVals <- members.toVector.traverse {
          case (name, value) => for {
            v <- compileExpr(value)
          } yield memberTypes.indexWhere(_.name == name) -> v
        }
        orderedMemberVals = memberVals.sortBy(_._1).map(_._2)
      } yield Val.Struct(orderedMemberVals)

    case Typed(ast.StringLit(v, _), BuiltinSymbols.StrType) =>
      val cpe = ConstantPoolEntry.Str(v)
      constants(cpe).as(Val.Struct(Vector(Val.Const(cpe, Type.Ptr), Val.I(v.length, Type.U32))))

    case Typed(ast.UnitLit(_), analyze.Type.U0) => Res.pure(Val.I(0, Type.U0))

    case Typed(ast.Var(n: Symbol.Local, _), _) => WriterT.lift(State.inspect(s => Val.R(s.registerBindings(n))))
    case Typed(ast.Var(Symbol.Global(path), _), t) =>
      for {
        r <- genReg(translateType(t))
        _ <- instructions(
          Inst.Move(r, Op.Load(Val.GlobalAddr(Name.Global(path)), Val.I(0, Type.I64)))
        )
      } yield Val.R(r)

    case Typed(ast.InfixAp(InfixOp.And, left, right, loc), analyze.Type.U1) =>
      compileExpr(Typed(ast.If(left, right, Some(Typed(ast.BoolLit(false, loc), analyze.Type.U1)), loc), analyze.Type.U1))

    case Typed(ast.InfixAp(InfixOp.Or, left, right, loc), analyze.Type.U1) =>
      compileExpr(Typed(ast.If(left, Typed(ast.BoolLit(true, loc), analyze.Type.U1), Some(right), loc), analyze.Type.U1))

    case Typed(ast.InfixAp(op, left, right, _), valType) =>
      for {
        lval <- compileExpr(left)
        rval <- compileExpr(right)
        res <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(res, Op.Binary(op, lval, rval))
        )
      } yield Val.R(res)

    case Typed(ast.PrefixAp(op, expr, _), valType) =>
      for {
        v <- compileExpr(expr)
        r <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(r, Op.Unary(op, v))
        )
      } yield Val.R(r)

    case Typed(ast.Widen(expr, _), valType) =>
      for {
        v <- compileExpr(expr)
        r <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(r, Op.Widen(v))
        )
      } yield Val.R(r)

    case Typed(ast.Trim(expr, _), valType) =>
      for {
        v <- compileExpr(expr)
        r <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(r, Op.Trim(v))
        )
      } yield Val.R(r)

    case Typed(ast.Reinterpret(expr, _), valType) =>
      for {
        v <- compileExpr(expr)
        r <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(r, Op.Copy(v))
        )
      } yield Val.R(r)

    case Typed(ast.Convert(src, _), valType) =>
      for {
        v <- compileExpr(src)
        r <- genReg(translateType(valType))
        _ <- instructions(
          Inst.Move(r, Op.Convert(v, r.typ))
        )
      } yield Val.R(r)

    case Typed(ast.Ignore(expr, _), analyze.Type.U0) =>
      for {
        _ <- compileExpr(expr)
      } yield Val.I(0, Type.U0)

    case Typed(ast.Block(stmnts, _), typ) =>
      for {
        bindings <- storeBindings
        vals <- stmnts.traverse {
          case t @ Typed(_: ast.Expression, _) => compileExpr(t.asInstanceOf[Typed[ast.Expression]])
          case Typed(ast.ValDef(name, _, expr), _) =>
            for {
              v <- compileExpr(expr)
              r <- genReg(translateType(expr.typ))
              _ <- instructions(
                Inst.Move(r, Op.Copy(v))
              )
              _ <- withBindings(name -> r)
            } yield Val.R(r): Val

          case Typed(ast.VarDef(name, _, expr), _) => // TODO deduplicate
            for {
              v <- compileExpr(expr)
              r <- genReg(translateType(expr.typ))
              _ <- instructions(
                Inst.Move(r, Op.Copy(v))
              )
              _ <- withBindings(name -> r)
            } yield Val.R(r): Val
        }
        _ <- restoreBindings(bindings)
      } yield vals.lastOption getOrElse Val.I(0, ir.Type.U0)

    case Typed(ast.If(cond, pos, neg, _), typ) =>
      for {
        condVal <- compileExpr(cond)
        lbli <- genLocalIndex
        trueLbl = Name.Local("if", lbli)
        falseLbl = Name.Local("else", lbli)
        afterLbl = Name.Local("ifafter", lbli)
        res <- genReg(translateType(typ))
        _ <- instructions(
          Inst.Flow(FlowControl.Branch(condVal, trueLbl, neg match {
            case Some(_) => falseLbl
            case None => afterLbl
          })
        ))

        _ <- instructions(
          Inst.Label(trueLbl)
        )
        trueVal <- compileExpr(pos)
        _ <- instructions(
          Inst.Move(res, Op.Copy(trueVal)),
          Inst.Flow(FlowControl.Goto(afterLbl))
        )

        _ <- neg.map { neg =>
          for {
            _ <- instructions(
              Inst.Label(falseLbl)
            )
            falseVal <- compileExpr(neg)
            _ <- instructions(
              Inst.Move(res, Op.Copy(falseVal)),
              Inst.Flow(FlowControl.Goto(afterLbl))
            )
          } yield ()
        } getOrElse Res.pure(())

        _ <- instructions(
          Inst.Label(afterLbl)
        )
      } yield Val.R(res)

    case Typed(ast.While(cond, body, _), _) =>
      for {
        lbli <- genLocalIndex
        condLbl = Name.Local("whilecond", lbli)
        bodyLbl = Name.Local("while", lbli)
        afterLbl = Name.Local("whileafter", lbli)
        _ <- instructions(
          Inst.Label(condLbl)
        )
        condVal <- compileExpr(cond)
        _ <- instructions(
          Inst.Flow(FlowControl.Branch(condVal, bodyLbl, afterLbl)),
          Inst.Label(bodyLbl)
        )
        _ <- compileExpr(body)
        _ <- instructions(
          Inst.Flow(FlowControl.Goto(condLbl)),
          Inst.Label(afterLbl)
        )
      } yield Val.I(0, ir.Type.U0)

    case Typed(ast.App(Typed(ast.Var(Symbol.Global(List("sqrt")), _), _), List(param), _), tpe) =>
      for {
        p <- compileExpr(param)
        r <- genReg(translateType(tpe))
        _ <- instructions(
          Inst.Move(r, Op.Sqrt(p))
        )
      } yield Val.R(r)

    case Typed(ast.App(Typed(fn, fnType: analyze.Type.Fun), params, _), t) =>
      for {
        fnVal <- fn match {
          case ast.Var(Symbol.Global(path), _) => Res.pure(Val.G(Name.Global(path), translateType(fnType)))
          case _ => compileExpr(Typed(fn, fnType))
        }
        paramVals <- params.traverse(compileExpr)
        paramRegs <- paramVals.traverse { p =>
          for {
            r <- genReg(p.typ)
            _ <- instructions(
              Inst.Move(r, Op.Copy(p))
            )
          } yield r
        }
        temp <- genReg(translateType(t))
        r <- genReg(translateType(t))
        _ <- instructions(
          Inst.Move(temp, Op.Call(fnVal, paramRegs)),
          Inst.Move(r, Op.Copy(Val.R(temp)))
        )
      } yield Val.R(r)

    case Typed(ast.App(Typed(ast.App(ptr @ Typed(_, analyze.Type.Ptr(_)), Nil, _), analyze.Type.Arr(member, _)), List(index), _), t) =>
      assert(member == t)
      for {
        ptrVal <- compileExpr(ptr)
        resType = translateType(member)
        offVal <- for {
          v <- compileExpr(index)
          r <- genReg(Type.I64)
          _ <- instructions(
            Inst.Move(r, Op.Binary(InfixOp.Mul, v, Val.I(resType.size, Type.I64)))
          )
        } yield Val.R(r)
        r <- genReg(resType)
        _ <- instructions(
          Inst.Move(r, Op.Load(ptrVal, offVal))
        )
      } yield Val.R(r)

    case Typed(ast.App(ptr @ Typed(_, analyze.Type.Ptr(_)), params, _), t) =>
      for {
        ptrv <- compileExpr(ptr)
        offv <- params.headOption.traverse { off =>
          for {
            v <- compileExpr(off)
            r <- genReg(Type.I64)
            _ <- instructions(
              Inst.Move(r, Op.Binary(InfixOp.Mul, v, Val.I(translateType(t).size, Type.I64)))
            )
          } yield Val.R(r)
        }
        r <- genReg(translateType(t))
        _ <- instructions(
          Inst.Move(r, Op.Load(ptrv, offv getOrElse Val.I(0, Type.I64)))
        )
      } yield Val.R(r)

    case Typed(ast.App(Typed(ast.Var(Symbol.Global(arrPath), _), analyze.Type.Arr(elemType, _)), index :: Nil, _), t) =>
      for {
        indexVal <- compileExpr(index)
        multipliedOffset <- genReg(Type.I64)
        r <- genReg(translateType(t))
        _ <- instructions(
          Inst.Move(multipliedOffset, Op.Binary(InfixOp.Mul, indexVal, Val.I(translateType(elemType).size, Type.I64))),
          Inst.Move(r, Op.Load(Val.GlobalAddr(Name.Global(arrPath)), Val.R(multipliedOffset)))
        )
      } yield Val.R(r)

    case Typed(ast.App(arr @ Typed(_, _: analyze.Type.Arr), index :: Nil, _), t) =>
      for {
        r <- genReg(translateType(t))
        a <- compileExpr(arr)
        i <- compileExpr(index)
        _ <- instructions(
          Inst.Move(r, Op.Elem(a, i))
        )
      } yield Val.R(r)

    case Typed(ast.Assign(Typed(ast.App(Typed(ast.App(ptr @ Typed(_, analyze.Type.Ptr(_)), Nil, _), analyze.Type.Arr(member, _)), List(index), _), _),
                          None, rval, _), _) =>
      for {
        right <- compileExpr(rval)
        ptrVal <- compileExpr(ptr)
        resType = translateType(member)
        offVal <- for {
          v <- compileExpr(index)
          r <- genReg(Type.I64)
          _ <- instructions(
            Inst.Move(r, Op.Binary(InfixOp.Mul, v, Val.I(resType.size, Type.I64)))
          )
        } yield Val.R(r)
        r <- genReg(resType)
        _ <- instructions(
          Inst.Do(Op.Store(ptrVal, offVal, right))
        )
      } yield Val.I(0, Type.U0)

    case Typed(ast.Assign(Typed(ast.Var(n, _), _), None, rval, _), _) =>
      for {
        right <- compileExpr(rval)
        dest <- WriterT.lift(CodegenState.inspect(_.registerBindings(n))): Res[Register]
        _ <- instructions(
          Inst.Move(dest, Op.Copy(right))
        )
      } yield Val.I(0, ir.Type.U0)

    case Typed(ast.Assign(Typed(ast.App(ptr @ Typed(_, analyze.Type.Ptr(pointee)), params, _), _), None, rval, _), _) =>
      // TODO pointee should be extracted from the pointer type, but Type.Ptr(_) contains an unresolved typename, some consideration is needed here
      for {
        right <- compileExpr(rval)
        ptrv <- compileExpr(ptr)
        offv <- params.headOption.traverse { v =>
          for {
            vv <- compileExpr(v)
            r <- genReg(Type.I64)
            _ <- instructions(
              Inst.Move(r, Op.Binary(InfixOp.Mul, vv, Val.I(translateType(pointee).size, Type.I64)))
            )
          } yield Val.R(r)
        }
        _ <- instructions(
          Inst.Do(Op.Store(ptrv, offv getOrElse Val.I(0, Type.I64), right))
        )
      } yield Val.I(0, ir.Type.U0)

    case Typed(ast.Assign(Typed(ast.Select(struct @ Typed(_, analyze.Type.Struct(_, members)), member, _), _), None, rval, _), _) =>
      for {
        right <- compileExpr(rval) // TODO handle changing struct behind a pointer `val p: ptr[str] = ???; p().length = 10`
        structVal <- compileExpr(struct)
        _ <- structVal match {
          case r: Val.R =>
            instructions(
              Inst.Move(r.register, Op.StructCopy(r, Map(members.indexWhere(_.name == member) -> right)))
            )
        }
      } yield Val.I(0, ir.Type.U0)

    case Typed(ast.Select(src @ Typed(_, analyze.Type.Ptr(structType: analyze.Type.Struct)), member, _), typ) =>
      for {
        srcv <- compileExpr(src)
        struct = translateType(structType).asInstanceOf[Type.Struct]
        r <- genReg(Type.Ptr)
        _ <- instructions(
          Inst.Move(r, Op.Binary(InfixOp.Add, srcv, Val.I(struct.offset(structType.indexOf(member)), Type.I64)))
        )
      } yield Val.R(r)

    case Typed(ast.Select(src @ Typed(_, structType: analyze.Type.Struct), member, _), typ) =>
      for {
        srcv <- compileExpr(src)
        r <- genReg(translateType(typ))
        _ <- instructions(
          Inst.Move(r, Op.Member(srcv, structType.indexOf(member)))
        )
      } yield Val.R(r)

    case Typed(ast.Stackalloc(pointee, _), _) =>
      for {
        r <- genReg(Type.Ptr)
        _ <- instructions(
          Inst.Move(r, Op.Stackalloc(translateType(pointee).size))
        )
      } yield Val.R(r)

    case Typed(ast.ArrLit(elems, _), arrType @ analyze.Type.Arr(_, size))
      if elems.size == size => elems.traverse(compileExpr).map(Val.Array)
  }

  def compileDef(d: ast.Def): Def = d match {
    case ast.DefDef(Symbol.Global(name), params, _, Typed(ast.Extern(_), ret)) =>
      val plist = params.getOrElse(Nil)
      Def.ExternFun(Name.Global(name), plist.map(p => translateType(p.typ)), translateType(ret))

    case ast.DefDef(Symbol.Global(name), params, _, body) =>
      val s: Res[(List[Register], ir.Register)] = for {
        paramRegs <- params.getOrElse(Seq.empty).toList
          .traverse(p => genReg(translateType(p.typ)).map(Symbol.Local(p.name) -> _))
        paramTemps <- paramRegs.traverse {
          case (name, r) => for {
            temp <- genReg(r.typ)
            _ <- instructions(
              Inst.Move(temp, Op.Copy(Val.R(r)))
            )
          } yield name -> temp
        }
        _ <- withBindings(paramTemps: _*)
        v <- compileExpr(body)
        vtemp <- genReg(translateType(body.typ))
        _ <- instructions(
          Inst.Move(vtemp, Op.Copy(v))
        )
      } yield (paramRegs.map(_._2), vtemp)
      val (Log(instrs, consts), (paramRegs, v)) = s.run.runA(CodegenState()).value
      val retType = translateType(body.typ)
      Def.Fun(Name.Global(name), paramRegs, retType, Vector(Block(Name.Local("body", 0), instrs, FlowControl.Return(ir.Val.R(v)))), consts.toSet)

    case ast.ValDef(Symbol.Global(name), _, value) =>
      val (log, valueVal) = compileExpr(value).run.runA(CodegenState()).value
      if(log.insts.isEmpty) {
        Def.TrivialVal(Name.Global(name), valueVal)
      } else Def.ComputedVal(Name.Global(name), Vector(Block(Name.Local("body", 0), log.insts, FlowControl.Return(valueVal))),
                             translateType(value.typ), log.consts.toSet)
  }

  private def translateType(t: analyze.Type): ir.Type = t match {
    case analyze.Type.I8 => ir.Type.I8
    case analyze.Type.I16 => ir.Type.I16
    case analyze.Type.I32 => ir.Type.I32
    case analyze.Type.I64 => ir.Type.I64
    case analyze.Type.U8 => ir.Type.U8
    case analyze.Type.U16 => ir.Type.U16
    case analyze.Type.U32 => ir.Type.U32
    case analyze.Type.U64 => ir.Type.U64

    case analyze.Type.F32 => ir.Type.F32
    case analyze.Type.F64 => ir.Type.F64

    case analyze.Type.U1 => ir.Type.U1
    case analyze.Type.U0 => ir.Type.U0

    case analyze.Type.Fun(params, ret) =>
      ir.Type.Fun(params.map(translateType), translateType(ret))

    case analyze.Type.Ptr(_) => ir.Type.Ptr

    case analyze.Type.Arr(member, size) => ir.Type.Arr(translateType(member), size)

    case analyze.Type.Struct(_, members) => ir.Type.Struct(members.map(m => translateType(m.typ)).toVector)

  }

  private[codegen] def genReg(t: Type): Res[Register] =
    WriterT.lift(CodegenState.genReg(t))

  private[codegen] def genLocalName(prefix: String): Res[Name] =
    WriterT.lift(CodegenState.genLocalName(prefix))

  private def genLocalIndex: Res[Int] =
    WriterT.lift(CodegenState.genLocalIndex)

  private[codegen] def withBindings(bindings: (Symbol, Register)*): Res[Unit] =
    WriterT.lift(CodegenState.withBindings(bindings: _*))

  private[codegen] def restoreBindings(bindings: Map[Symbol, Register]): Res[Unit] =
    WriterT.lift(State.modify(_.copy(registerBindings = bindings)))

  private[codegen] def storeBindings: Res[Map[Symbol, Register]] =
    WriterT.lift(State.inspect(_.registerBindings))

}
