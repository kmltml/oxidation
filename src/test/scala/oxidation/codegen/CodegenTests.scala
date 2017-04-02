package oxidation
package codegen

import analyze.{BuiltinSymbols, TypedSyntax, ast}
import analyze.Type._
import ir._
import utest._

object CodegenTests extends TestSuite with TypedSyntax with SymbolSyntax with IrValSyntax {

  import Codegen.register

  private def r(i: Int, t: ir.Type.type => ir.Type): Register = register(i, t(ir.Type))

  private def insts(insts: Inst*): Codegen.Log = Codegen.Log(insts.toVector)

  val tests = apply {
    "compileExpr" - {
      import Codegen.compileExpr
      "IntLit" - {
        compileExpr(ast.IntLit(20) :: I32)
          .run.runA(CodegenState()).value ==> (insts(), Val.I(20, ir.Type.I32))
      }
      "BoolLit" - {
        compileExpr(ast.BoolLit(true) :: U1).run.runA(CodegenState()).value ==>
          (insts(), Val.I(1, ir.Type.U1))
        compileExpr(ast.BoolLit(false) :: U1).run.runA(CodegenState()).value ==>
          (insts(), Val.I(0, ir.Type.U1))
      }
      "CharLit" - {
        compileExpr(ast.CharLit('a') :: U8).run.runA(CodegenState()).value ==>
          (insts(), Val.I(97, ir.Type.U8))
      }
      "StructLit" - {
        compileExpr(ast.StructLit(g('str), Seq(
          "data" -> (ast.Var(l('x)) :: Ptr(TypeName.Named(g('u8)))),
          "length" -> (ast.IntLit(10) :: U32)
        )) :: BuiltinSymbols.StrType).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.Ptr)), nextReg = 1)).value ==>
          (insts(), Val.Struct(Vector(r(0, _.Ptr), Val.I(10, ir.Type.U32))))
      }
      "StringLit" - {
        compileExpr(ast.StringLit("Hello, world!") :: BuiltinSymbols.StrType).run.runA(CodegenState()).value ==>
          (Codegen.Log(Vector.empty, Vector(ConstantPoolEntry.Str("Hello, world!"))),
            Val.Struct(Vector(Val.Const(ConstantPoolEntry.Str("Hello, world!"), ir.Type.Ptr), Val.I(13, ir.Type.U32))))
      }
      "UnitLit" - {
        compileExpr(ast.UnitLit() :: U0).run.runA(CodegenState()).value ==>
          (Codegen.Log(Vector.empty), Val.I(0, ir.Type.U0))
      }
      "InfixAp" - {
        compileExpr(ast.InfixAp(InfixOp.Add, ast.IntLit(1) :: I32, ast.IntLit(2) :: I32) :: I32)
          .run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.I32), Op.Arith(InfixOp.Add, 1, 2))
          ), Val.R(r(0, _.I32)))

        compileExpr(ast.InfixAp(InfixOp.Add,
          ast.InfixAp(InfixOp.Sub, ast.IntLit(3) :: I32, ast.IntLit(2) :: I32) :: I32,
          ast.IntLit(1) :: I32) :: I32).run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.I32), Op.Arith(InfixOp.Sub, 3, 2)),
            Inst.Move(r(1, _.I32), Op.Arith(InfixOp.Add, r(0, _.I32), 1))
          ), Val.R(r(1, _.I32)))
      }
      "PrefixAp" - {
        compileExpr(ast.PrefixAp(PrefixOp.Neg, ast.IntLit(20) :: I32) :: I32)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Unary(PrefixOp.Neg, 20))
          ), Val.R(r(0, _.I32)))
      }
      "Widen" - {
        compileExpr(ast.Widen(ast.Var(l('x)) :: I32) :: I64)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.I64), Op.Widen(r(0, _.I32)))
          ), Val.R(r(1, _.I64)))
      }
      "Ignore" - {
        compileExpr(ast.Ignore(ast.App(ast.Var(g('foo)) :: Fun(Seq(I32), I32), Seq(ast.IntLit(42) :: I32)) :: I32) :: U0)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Copy(42)),
            Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(List(ir.Type.I32), ir.Type.I32)), List(r(0, _.I32))))
          ), Val.I(0, ir.Type.U0))
      }
      "Var" - {
        compileExpr(ast.Var(l('x)) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)))).value ==>
          (insts(), Val.R(r(0, _.I32)))
      }
      "Block" - {
        compileExpr(ast.Block(Vector(
          ast.ValDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.ValDef(l('y), None, ast.IntLit(20) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.Var(l('y)) :: I32) :: I32
        )) :: I32).run.runA(CodegenState()).value ==> (insts(
          Inst.Move(r(0, _.I32), Op.Copy(10)),
          Inst.Move(r(1, _.I32), Op.Copy(20)),
          Inst.Move(r(2, _.I32), Op.Arith(InfixOp.Add, r(0, _.I32), r(1, _.I32)))
        ), Val.R(r(2, _.I32)))
      }
      "If" - {
        "with else branch" - {
          compileExpr(ast.If(ast.Var(l('x)) :: U1, ast.IntLit(10) :: I32, Some(ast.IntLit(20) :: I32)) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
            (insts(
              Inst.Flow(FlowControl.Branch(r(0, _.U1), Name.Local("if", 0), Name.Local("else", 0))),

              Inst.Label(Name.Local("if", 0)),
              Inst.Move(r(1, _.I32), Op.Copy(10)),
              Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("else", 0)),
              Inst.Move(r(1, _.I32), Op.Copy(20)),
              Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("ifafter", 0))
            ), Val.R(r(1, _.I32)))
        }
        "whithout else branch" - {
          val fooType = Fun(Seq.empty, U0)
          compileExpr(ast.If(ast.Var(l('x)) :: U1,
            ast.App(ast.Var(g('foo)) :: fooType, Seq.empty) :: U0, None) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
            (insts(
              Inst.Flow(FlowControl.Branch(r(0, _.U1), Name.Local("if", 0), Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("if", 0)),
              Inst.Move(r(2, _.U0), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(Nil, ir.Type.U0)), List.empty)),
              Inst.Move(r(1, _.U0), Op.Copy(r(2, _.U0))),
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
          (insts(
            Inst.Label(Name.Local("whilecond", 0)),
            Inst.Move(r(1, _.U1), Op.Arith(InfixOp.Lt, r(0, _.I32), 10)),
            Inst.Flow(FlowControl.Branch(r(1, _.U1), Name.Local("while", 0), Name.Local("whileafter", 0))),

            Inst.Label(Name.Local("while", 0)),
            Inst.Move(r(2, _.I32), Op.Arith(InfixOp.Add, r(0, _.I32), 1)),
            Inst.Move(r(0, _.I32), Op.Copy(r(2, _.I32))),
            Inst.Flow(FlowControl.Goto(Name.Local("whilecond", 0))),

            Inst.Label(Name.Local("whileafter", 0))
          ), Val.I(0, ir.Type.U0))
      }
      "Assign" - {
        "variable" - {
          compileExpr(ast.Assign(ast.Var(l('x)) :: I32, None, ast.IntLit(20) :: I32) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Copy(20))
            ), Val.I(0, ir.Type.U0))
        }
        "ptr" - {
          compileExpr(ast.Assign(ast.App(ast.Var(l('p)) :: Ptr(TypeName.Named(g('i32))), List(ast.IntLit(8) :: I32)) :: I32,
            None, ast.IntLit(20) :: I32) :: U0).run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Do(Op.Store(r(0, _.Ptr), 8, 20))
            ), Val.I(0, ir.Type.U0))
        }
      }
      "Select" - {
        "Member" - {
          val fooStruct = Struct(g('foo), Seq(
            StructMember("x", I32),
            StructMember("y", I64)
          ))
          val r0 = register(0, ir.Type.Struct(Vector(ir.Type.I32, ir.Type.I64)))
          compileExpr(ast.Select(ast.Var(l('x)) :: fooStruct, "y") :: I64)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Member(r0, 1))
            ), Val.R(r(1, _.I64)))
        }
      }
      "App" - {
        "function" - {
          compileExpr(ast.App(ast.Var(g('f)) :: Fun(Seq(I32), U1), Seq(ast.IntLit(10) :: I32)) :: I32)
            .run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Copy(10)),
              Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("f")), ir.Type.Fun(List(ir.Type.I32), ir.Type.U1)), List(r(0, _.I32))))
            ), Val.R(r(1, _.I32)))
        }
        "pointer" - {
          compileExpr(ast.App(ast.Var(l('p)) :: Ptr(TypeName.Named(g('i32))), Nil) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I32), Op.Load(r(0, _.Ptr), Val.I(0, ir.Type.I64)))
            ), Val.R(r(1, _.I32)))
        }
      }
    }
    "compileDef" - {
      import Codegen.compileDef
      "DefDef" - {
        "Extern" - {
          compileDef(ast.DefDef(g('mod, 'foo), Some(List(ast.Param("x", I64))), Some(TypeName.Named(g('i32))), ast.Extern() :: I32)) ==>
            Def.ExternFun(Name.Global(List("mod", "foo")), List(ir.Type.I64), ir.Type.I32)
        }
        "Collects string literals" - {
          val strStruct = ir.Type.Struct(Vector(ir.Type.Ptr, ir.Type.U32))
          compileDef(ast.DefDef(g('foo), None, None, ast.Block(Vector(
            ast.ValDef(l('x), None, ast.StringLit("one") :: BuiltinSymbols.StrType) :: U0,
            ast.ValDef(l('y), None, ast.StringLit("two") :: BuiltinSymbols.StrType) :: U0,
            ast.ValDef(l('z), None, ast.StringLit("two") :: BuiltinSymbols.StrType) :: U0,
            ast.IntLit(10) :: I32
          )) :: I32)) ==>
            Def.Fun(Name.Global(List("foo")), Nil, ir.Type.I32, Vector(
              Block(Name.Local("body", 0), Vector(
                Inst.Move(register(0, strStruct), Op.Copy(Val.Struct(Vector(Val.Const(ConstantPoolEntry.Str("one"), ir.Type.Ptr), Val.I(3, ir.Type.U32))))),
                Inst.Move(register(1, strStruct), Op.Copy(Val.Struct(Vector(Val.Const(ConstantPoolEntry.Str("two"), ir.Type.Ptr), Val.I(3, ir.Type.U32))))),
                Inst.Move(register(2, strStruct), Op.Copy(Val.Struct(Vector(Val.Const(ConstantPoolEntry.Str("two"), ir.Type.Ptr), Val.I(3, ir.Type.U32))))),
                Inst.Move(r(3, _.I32), Op.Copy(10))
              ), FlowControl.Return(r(3, _.I32)))
            ), Set(
              ConstantPoolEntry.Str("one"), ConstantPoolEntry.Str("two")
            ))
        }
      }
    }
  }

}
