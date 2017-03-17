package oxidation
package codegen

import analyze.{TypedSyntax, ast}
import analyze.Type._
import ir._
import utest._

object CodegenTests extends TestSuite with TypedSyntax with SymbolSyntax {

  private def r(i: Int, t: ir.Type.type => ir.Type): Register = Register(i, t(ir.Type))

  private implicit def vali(i: Int): Val = Val.I(i)
  private implicit def valr(r: Register): Val = Val.R(r)


  val tests = apply {
    "compileExpr" - {
      import Codegen.compileExpr
      "IntLit" - {
        compileExpr(ast.IntLit(20) :: I32)
          .run.runA(CodegenState()).value ==> (Vector.empty, Val.I(20))
      }
      "BoolLit" - {
        compileExpr(ast.BoolLit(true) :: U1).run.runA(CodegenState()).value ==>
          (Vector.empty, Val.I(1))
        compileExpr(ast.BoolLit(false) :: U1).run.runA(CodegenState()).value ==>
          (Vector.empty, Val.I(0))
      }
      "InfixOp" - {
        compileExpr(ast.InfixAp(InfixOp.Add, ast.IntLit(1) :: I32, ast.IntLit(2) :: I32) :: I32)
          .run.runA(CodegenState()).value ==> (Vector(
            Inst.Eval(Some(r(0, _.I32)), Op.Arith(InfixOp.Add, 1, 2))
          ), Val.R(r(0, _.I32)))

        compileExpr(ast.InfixAp(InfixOp.Add,
          ast.InfixAp(InfixOp.Sub, ast.IntLit(3) :: I32, ast.IntLit(2) :: I32) :: I32,
          ast.IntLit(1) :: I32) :: I32).run.runA(CodegenState()).value ==> (Vector(
            Inst.Eval(Some(r(0, _.I32)), Op.Arith(InfixOp.Sub, 3, 2)),
            Inst.Eval(Some(r(1, _.I32)), Op.Arith(InfixOp.Add, r(0, _.I32), 1))
          ), Val.R(r(1, _.I32)))
      }
      "Var" - {
        compileExpr(ast.Var(l('x)) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)))).value ==>
          (Vector.empty, Val.R(r(0, _.I32)))
      }
      "Block" - {
        compileExpr(ast.Block(Vector(
          ast.ValDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.ValDef(l('y), None, ast.IntLit(20) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.Var(l('y)) :: I32) :: I32
        )) :: I32).run.runA(CodegenState()).value ==> (Vector(
          Inst.Eval(Some(r(0, _.I32)), Op.Copy(10)),
          Inst.Eval(Some(r(1, _.I32)), Op.Copy(20)),
          Inst.Eval(Some(r(2, _.I32)), Op.Arith(InfixOp.Add, r(0, _.I32), r(1, _.I32)))
        ), Val.R(r(2, _.I32)))
      }
      "If" - {
        "with else branch" - {
          compileExpr(ast.If(ast.Var(l('x)) :: U1, ast.IntLit(10) :: I32, Some(ast.IntLit(20) :: I32)) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
            (Vector(
              Inst.Flow(FlowControl.Branch(r(0, _.U1), Name.Local("if", 0), Name.Local("else", 0))),

              Inst.Label(Name.Local("if", 0)),
              Inst.Eval(Some(r(1, _.I32)), Op.Copy(10)),
              Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("else", 0)),
              Inst.Eval(Some(r(1, _.I32)), Op.Copy(20)),
              Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("ifafter", 0))
            ), Val.R(r(1, _.I32)))
        }
        "whithout else branch" - {
          val fooType = Fun(Seq.empty, U0)
          compileExpr(ast.If(ast.Var(l('x)) :: U1,
            ast.App(ast.Var(g('foo)) :: fooType, Seq.empty) :: U0, None) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
            (Vector(
              Inst.Flow(FlowControl.Branch(r(0, _.U1), Name.Local("if", 0), Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("if", 0)),
              Inst.Eval(Some(r(2, _.U0)), Op.Call(Val.G(Name.Global(List("foo"))), List.empty)),
              Inst.Eval(Some(r(1, _.U0)), Op.Copy(r(2, _.U0))),
              Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("ifafter", 0))
            ), Val.R(r(1, _.U0)))
        }
      }
      "While" - {
        compileExpr(ast.While(
          ast.InfixAp(InfixOp.Lt, ast.Var(l('x)) :: I32, ast.IntLit(10) :: I32) :: U1,
          ast.Assign(ast.Var(l('x)) :: I32, None,
            ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.IntLit(1) :: I32) :: I32) :: U0
        ) :: U0).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (Vector(
            Inst.Label(Name.Local("whilecond", 0)),
            Inst.Eval(Some(r(1, _.U1)), Op.Arith(InfixOp.Lt, r(0, _.I32), 10)),
            Inst.Flow(FlowControl.Branch(r(1, _.U1), Name.Local("while", 0), Name.Local("whileafter", 0))),

            Inst.Label(Name.Local("while", 0)),
            Inst.Eval(Some(r(2, _.I32)), Op.Arith(InfixOp.Add, r(0, _.I32), 1)),
            Inst.Eval(Some(r(0, _.I32)), Op.Copy(r(2, _.I32))),
            Inst.Flow(FlowControl.Goto(Name.Local("whilecond", 0))),

            Inst.Label(Name.Local("whileafter", 0))
          ), Val.I(0))
      }
      "Assign" - {
        compileExpr(ast.Assign(ast.Var(l('x)) :: I32, None, ast.IntLit(20) :: I32) :: U0)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (Vector(
            Inst.Eval(Some(r(0, _.I32)), Op.Copy(20))
          ), Val.I(0))
      }
      "App" - {
        "function" - {
          compileExpr(ast.App(ast.Var(g('f)) :: Fun(Seq(I32), U1), Seq(ast.IntLit(10) :: I32)) :: I32)
            .run.runA(CodegenState()).value ==>
            (Vector(
              Inst.Eval(Some(r(0, _.I32)), Op.Call(Val.G(Name.Global(List("f"))), List(10)))
            ), Val.R(r(0, _.I32)))
        }
      }
    }
  }

}
