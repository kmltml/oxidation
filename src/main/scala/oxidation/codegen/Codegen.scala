package oxidation
package codegen

import analyze.{Typed, ast}
import ir._

import cats._
import cats.data._
import cats.implicits._

object Codegen {

  type Res[A] = WriterT[State[CodegenState, ?], Vector[Inst], A]
  private val Res = MonadWriter[Res, Vector[Inst]]

  object CodegenReg extends RegisterNamespace {
    override def prefix: String = "r"
  }

  def register(index: Int, typ: ir.Type): ir.Register = ir.Register(CodegenReg, index, typ)

  def compileExpr(expr: Typed[ast.Expression]): Res[Val] = expr match {
    case Typed(ast.IntLit(i), typ) => Res.pure(Val.I(i, translateType(typ)))
    case Typed(ast.BoolLit(b), typ) =>
      val i = b match {
        case true => 1
        case false => 0
      }
      Res.pure(Val.I(i, translateType(typ)))
    case Typed(ast.Var(n), _) => WriterT.lift(State.inspect(s => Val.R(s.registerBindings(n))))
    case Typed(ast.InfixAp(op, left, right), valType) =>
      for {
        lval <- compileExpr(left)
        rval <- compileExpr(right)
        res <- genReg(translateType(valType))
        _ <- Res.tell(Vector(
          Inst.Move(res, Op.Arith(op, lval, rval))
        ))
      } yield Val.R(res)

    case Typed(ast.PrefixAp(op, expr), valType) =>
      for {
        v <- compileExpr(expr)
        r <- genReg(translateType(valType))
        _ <- Res.tell(Vector(
          Inst.Move(r, Op.Unary(op, v))
        ))
      } yield Val.R(r)

    case Typed(ast.Block(stmnts), typ) =>
      for {
        bindings <- storeBindings
        vals <- stmnts.toVector.traverse {
          case t @ Typed(_: ast.Expression, _) => compileExpr(t.asInstanceOf[Typed[ast.Expression]])
          case Typed(ast.ValDef(name, _, expr), _) =>
            for {
              v <- compileExpr(expr)
              r <- genReg(translateType(expr.typ))
              _ <- Res.tell(Vector(
                Inst.Move(r, Op.Copy(v))
              ))
              _ <- withBindings(name -> r)
            } yield Val.R(r): Val

          case Typed(ast.VarDef(name, _, expr), _) => // TODO deduplicate
            for {
              v <- compileExpr(expr)
              r <- genReg(translateType(expr.typ))
              _ <- Res.tell(Vector(
                Inst.Move(r, Op.Copy(v))
              ))
              _ <- withBindings(name -> r)
            } yield Val.R(r): Val
        }
        _ <- restoreBindings(bindings)
      } yield vals.lastOption getOrElse Val.I(0, ir.Type.U0)

    case Typed(ast.If(cond, pos, neg), typ) =>
      for {
        condVal <- compileExpr(cond)
        lbli <- genLocalIndex
        trueLbl = Name.Local("if", lbli)
        falseLbl = Name.Local("else", lbli)
        afterLbl = Name.Local("ifafter", lbli)
        res <- genReg(translateType(typ))
        _ <- Res.tell(Vector(
          Inst.Flow(FlowControl.Branch(condVal, trueLbl, neg match {
            case Some(_) => falseLbl
            case None => afterLbl
          }))
        ))

        _ <- Res.tell(Vector(
          Inst.Label(trueLbl)
        ))
        trueVal <- compileExpr(pos)
        _ <- Res.tell(Vector(
          Inst.Move(res, Op.Copy(trueVal)),
          Inst.Flow(FlowControl.Goto(afterLbl))
        ))

        _ <- neg.map { neg =>
          for {
            _ <- Res.tell(Vector(
              Inst.Label(falseLbl)
            ))
            falseVal <- compileExpr(neg)
            _ <- Res.tell(Vector(
              Inst.Move(res, Op.Copy(falseVal)),
              Inst.Flow(FlowControl.Goto(afterLbl))
            ))
          } yield ()
        } getOrElse Res.pure(())

        _ <- Res.tell(Vector(
          Inst.Label(afterLbl)
        ))
      } yield Val.R(res)

    case Typed(ast.While(cond, body), _) =>
      for {
        lbli <- genLocalIndex
        condLbl = Name.Local("whilecond", lbli)
        bodyLbl = Name.Local("while", lbli)
        afterLbl = Name.Local("whileafter", lbli)
        _ <- Res.tell(Vector(
          Inst.Label(condLbl)
        ))
        condVal <- compileExpr(cond)
        _ <- Res.tell(Vector(
          Inst.Flow(FlowControl.Branch(condVal, bodyLbl, afterLbl)),
          Inst.Label(bodyLbl)
        ))
        _ <- compileExpr(body)
        _ <- Res.tell(Vector(
          Inst.Flow(FlowControl.Goto(condLbl)),
          Inst.Label(afterLbl)
        ))
      } yield Val.I(0, ir.Type.U0)

    case Typed(ast.App(Typed(fn, fnType: analyze.Type.Fun), params), t) =>
      for {
        fnVal <- fn match {
          case ast.Var(Symbol.Global(path)) => Res.pure(Val.G(Name.Global(path.toList), translateType(fnType)))
          case _ => compileExpr(Typed(fn, fnType))
        }
        paramVals <- params.toList.traverse { p =>
          for {
            v <- compileExpr(p)
            r <- genReg(translateType(p.typ))
            _ <- Res.tell(Vector(
              Inst.Move(r, Op.Copy(v))
            ))
          } yield r
        }
        r <- genReg(translateType(t))
        _ <- Res.tell(Vector(
          Inst.Move(r, Op.Call(fnVal, paramVals))
        ))
      } yield Val.R(r)

    case Typed(ast.Assign(Typed(ast.Var(n), _), None, rval), _) =>
      for {
        right <- compileExpr(rval)
        dest <- WriterT.lift(CodegenState.inspect(_.registerBindings(n))): Res[Register]
        _ <- Res.tell(Vector(
          Inst.Move(dest, Op.Copy(right))
        ))
      } yield Val.I(0, ir.Type.U0)
  }

  def compileDef(d: ast.Def): Def = d match {
    case ast.DefDef(Symbol.Global(name), params, _, body) =>
      val s: Res[(List[Register], ir.Register)] = for {
        paramRegs <- params.getOrElse(Seq.empty).toList
          .traverse(p => genReg(translateType(p.typ)).map(Symbol.Local(p.name) -> _))
        paramTemps <- paramRegs.traverse {
          case (name, r) => for {
            temp <- genReg(r.typ)
            _ <- Res.tell(Vector(
              Inst.Move(temp, Op.Copy(Val.R(r)))
            ))
          } yield name -> temp
        }
        _ <- withBindings(paramTemps: _*)
        v <- compileExpr(body)
        vtemp <- genReg(translateType(body.typ))
        _ <- Res.tell(Vector(
          Inst.Move(vtemp, Op.Copy(v))
        ))
      } yield (paramRegs.map(_._2), vtemp)
      val (instrs, (paramRegs, v)) = s.run.runA(CodegenState()).value
      val retType = translateType(body.typ)
      Def.Fun(Name.Global(name.toList), paramRegs, retType, Vector(Block(Name.Local("body", 0), instrs, FlowControl.Return(ir.Val.R(v)))))
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

    case analyze.Type.U1 => ir.Type.U1
    case analyze.Type.U0 => ir.Type.U0


    case analyze.Type.Fun(params, ret) =>
      ir.Type.Fun(params.toList.map(translateType), translateType(ret))

//    case analyze.Type.Struct(name, members) =>

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
