package oxidation
package codegen

import analyze.{Typed, ast}
import ir._

import cats._
import cats.data._
import cats.implicits._

object Codegen {

  type Res[A] = WriterT[State[CodegenState, ?], Vector[Inst], A]
  val Res = MonadWriter[Res, Vector[Inst]]

  def compileExpr(expr: Typed[ast.Expression]): Res[Val] = expr match {
    case Typed(ast.IntLit(i), _) => Res.pure(Val.I(i))
    case Typed(ast.Var(n), _) => WriterT.lift(State.inspect(s => Val.R(s.registerBindings(n))))
    case Typed(ast.InfixAp(op, left, right), _) =>
      for {
        lval <- compileExpr(left)
        rval <- compileExpr(right)
        res <- genReg
        _ <- Res.tell(Vector(
          Inst.Eval(Some(res), Op.Arith(op, lval, rval))
        ))
      } yield Val.R(res)
  }

  def compileDef(d: ast.Def): Def = d match {
    case ast.DefDef(Symbol.Global(name), params, _, body) =>
      val s: Res[(List[Register], Val)] = for {
        paramRegs <- params.getOrElse(Seq.empty).toList.traverse(p => genReg.map(Symbol.Local(p.name) -> _))
        _ <- withBindings(paramRegs: _*)
        v <- compileExpr(body)
      } yield (paramRegs.map(_._2), v)
      val (instrs, (paramRegs, v)) = s.run.runA(CodegenState()).value
      Def.Fun(Name.Global(name.toList), paramRegs, Vector(Block(Name.Local("body", 0), instrs, FlowControl.Return(v))))
  }

  private[codegen] def genReg: Res[Register] =
    WriterT.lift(CodegenState.genReg)

  private[codegen] def withBindings(bindings: (Symbol, ir.Register)*): Res[Unit] =
    WriterT.lift(CodegenState.withBindings(bindings: _*))

}
