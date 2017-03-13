package oxidation
package codegen

import analyze.{TypedSyntax, ast}
import analyze.Type._
import ir._
import utest._

object CodegenTests extends TestSuite with TypedSyntax with SymbolSyntax {

  val tests = apply {
    "compileExpr" - {
      import Codegen.compileExpr
      val r0 = Register(0)
      val r1 = Register(1)
      "IntLit" - {
        compileExpr(ast.IntLit(20) :: I32)
          .run.runA(CodegenState()).value ==> (Vector.empty, Val.I(20))
      }
      "InfixOp" - {
        compileExpr(ast.InfixAp(InfixOp.Add, ast.IntLit(1) :: I32, ast.IntLit(2) :: I32) :: I32)
          .run.runA(CodegenState()).value ==> (Vector(
            Inst.Eval(Some(r0), Op.Arith(InfixOp.Add, Val.I(1), Val.I(2)))
          ), Val.R(r0))

        compileExpr(ast.InfixAp(InfixOp.Add,
          ast.InfixAp(InfixOp.Sub, ast.IntLit(3) :: I32, ast.IntLit(2) :: I32) :: I32,
          ast.IntLit(1) :: I32) :: I32).run.runA(CodegenState()).value ==> (Vector(
            Inst.Eval(Some(r0), Op.Arith(InfixOp.Sub, Val.I(3), Val.I(2))),
            Inst.Eval(Some(r1), Op.Arith(InfixOp.Add, Val.R(r0), Val.I(1)))
          ), Val.R(r1))
      }
      "Var" - {
        compileExpr(ast.Var(l('x)) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r0))).value ==>
          (Vector.empty, Val.R(r0))
      }
    }
  }

}
