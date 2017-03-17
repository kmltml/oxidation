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

  def compileExpr(expr: Typed[ast.Expression]): Res[Val] = expr match {
    case Typed(ast.IntLit(i), _) => Res.pure(Val.I(i))
    case Typed(ast.BoolLit(b), _) =>
      val i = b match {
        case true => 1
        case false => 0
      }
      Res.pure(Val.I(i))
    case Typed(ast.Var(n), _) => WriterT.lift(State.inspect(s => Val.R(s.registerBindings(n))))
    case Typed(ast.InfixAp(op, left, right), valType) =>
      for {
        lval <- compileExpr(left)
        rval <- compileExpr(right)
        res <- genReg(translateType(valType))
        _ <- Res.tell(Vector(
          Inst.Eval(Some(res), Op.Arith(op, lval, rval))
        ))
      } yield Val.R(res)

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
                Inst.Eval(Some(r), Op.Copy(v))
              ))
              _ <- withBindings(name -> r)
            } yield Val.R(r): Val
        }
        _ <- restoreBindings(bindings)
      } yield vals.lastOption getOrElse Val.I(0)

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
          Inst.Eval(Some(res), Op.Copy(trueVal)),
          Inst.Flow(FlowControl.Goto(afterLbl))
        ))

        _ <- neg.map { neg =>
          for {
            _ <- Res.tell(Vector(
              Inst.Label(falseLbl)
            ))
            falseVal <- compileExpr(neg)
            _ <- Res.tell(Vector(
              Inst.Eval(Some(res), Op.Copy(falseVal)),
              Inst.Flow(FlowControl.Goto(afterLbl))
            ))
          } yield ()
        } getOrElse Res.pure(())

        _ <- Res.tell(Vector(
          Inst.Label(afterLbl)
        ))
      } yield Val.R(res)

    case Typed(ast.App(Typed(fn, fnType: analyze.Type.Fun), params), t) =>
      for {
        fnVal <- fn match {
          case ast.Var(Symbol.Global(path)) => Res.pure(Val.G(Name.Global(path.toList)))
          case _ => compileExpr(Typed(fn, fnType))
        }
        paramVals <- params.toList.traverse(compileExpr)
        r <- genReg(translateType(t))
        _ <- Res.tell(Vector(
          Inst.Eval(Some(r), Op.Call(fnVal, paramVals))
        ))
      } yield Val.R(r)
  }

  def compileDef(d: ast.Def): Def = d match {
    case ast.DefDef(Symbol.Global(name), params, _, body) =>
      val s: Res[(List[Register], Val)] = for {
        paramRegs <- params.getOrElse(Seq.empty).toList
          .traverse(p => genReg(translateType(p.typ)).map(Symbol.Local(p.name) -> _))
        _ <- withBindings(paramRegs: _*)
        v <- compileExpr(body)
      } yield (paramRegs.map(_._2), v)
      val (instrs, (paramRegs, v)) = s.run.runA(CodegenState()).value
      val retType = translateType(body.typ)
      Def.Fun(Name.Global(name.toList), paramRegs, retType, Vector(Block(Name.Local("body", 0), instrs, FlowControl.Return(v))))
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


//    case analyze.Type.Fun(params, ret) =>

//    case analyze.Type.Struct(name, members) =>

  }

  private[codegen] def genReg(t: Type): Res[Register] =
    WriterT.lift(CodegenState.genReg(t))

  private[codegen] def genLocalName(prefix: String): Res[Name] =
    WriterT.lift(CodegenState.genLocalName(prefix))

  private def genLocalIndex: Res[Int] =
    WriterT.lift(CodegenState.genLocalIndex)

  private[codegen] def withBindings(bindings: (Symbol, ir.Register)*): Res[Unit] =
    WriterT.lift(CodegenState.withBindings(bindings: _*))

  private[codegen] def restoreBindings(bindings: Map[Symbol, ir.Register]): Res[Unit] =
    WriterT.lift(State.modify(_.copy(registerBindings = bindings)))

  private[codegen] def storeBindings: Res[Map[Symbol, ir.Register]] =
    WriterT.lift(State.inspect(_.registerBindings))

}
