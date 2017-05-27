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
      "FloatLit" - {
        "F32" - {
          compileExpr(ast.FloatLit(BigDecimal(0.1f)) :: F32)
            .run.runA(CodegenState()).value ==> (insts(), Val.F32(0.1f))
        }
        "F64" - {
          compileExpr(ast.FloatLit(BigDecimal(0.1)) :: F64)
            .run.runA(CodegenState()).value ==> (insts(), Val.F64(0.1))
        }
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
        compileExpr(ast.StructLit(g('str), List(
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
            Inst.Move(r(0, _.I32), Op.Binary(InfixOp.Add, 1, 2))
          ), Val.R(r(0, _.I32)))

        compileExpr(ast.InfixAp(InfixOp.Add,
          ast.InfixAp(InfixOp.Sub, ast.IntLit(3) :: I32, ast.IntLit(2) :: I32) :: I32,
          ast.IntLit(1) :: I32) :: I32).run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.I32), Op.Binary(InfixOp.Sub, 3, 2)),
            Inst.Move(r(1, _.I32), Op.Binary(InfixOp.Add, r(0, _.I32), 1))
          ), Val.R(r(1, _.I32)))
        "Or" - {
          compileExpr(ast.InfixAp(InfixOp.Or,
            ast.App(ast.Var(g('foo)) :: Fun(Nil, U1), Nil) :: U1,
            ast.App(ast.Var(g('bar)) :: Fun(Nil, U1), Nil) :: U1
          ) :: U1).run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.U1), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(Nil, ir.Type.U1)), Nil)),
            Inst.Move(r(1, _.U1), Op.Copy(r(0, _.U1))),
            Inst.Flow(FlowControl.Branch(Val.R(r(1, _.U1)), Name.Local("if", 0), Name.Local("else", 0))),

            Inst.Label(Name.Local("if", 0)),
            Inst.Move(r(2, _.U1), Op.Copy(Val.I(1, ir.Type.U1))),
            Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

            Inst.Label(Name.Local("else", 0)),
            Inst.Move(r(3, _.U1), Op.Call(Val.G(Name.Global(List("bar")), ir.Type.Fun(Nil, ir.Type.U1)), Nil)),
            Inst.Move(r(4, _.U1), Op.Copy(Val.R(r(3, _.U1)))),
            Inst.Move(r(2, _.U1), Op.Copy(Val.R(r(4, _.U1)))),
            Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

            Inst.Label(Name.Local("ifafter", 0))

          ), Val.R(r(2, _.U1)))
        }
        "And" - {
          compileExpr(ast.InfixAp(InfixOp.And,
            ast.App(ast.Var(g('foo)) :: Fun(Nil, U1), Nil) :: U1,
            ast.App(ast.Var(g('bar)) :: Fun(Nil, U1), Nil) :: U1
          ) :: U1).run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.U1), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(Nil, ir.Type.U1)), Nil)),
            Inst.Move(r(1, _.U1), Op.Copy(r(0, _.U1))),
            Inst.Flow(FlowControl.Branch(Val.R(r(1, _.U1)), Name.Local("if", 0), Name.Local("else", 0))),

            Inst.Label(Name.Local("if", 0)),
            Inst.Move(r(3, _.U1), Op.Call(Val.G(Name.Global(List("bar")), ir.Type.Fun(Nil, ir.Type.U1)), Nil)),
            Inst.Move(r(4, _.U1), Op.Copy(Val.R(r(3, _.U1)))),
            Inst.Move(r(2, _.U1), Op.Copy(Val.R(r(4, _.U1)))),
            Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

            Inst.Label(Name.Local("else", 0)),
            Inst.Move(r(2, _.U1), Op.Copy(Val.I(0, ir.Type.U1))),
            Inst.Flow(FlowControl.Goto(Name.Local("ifafter", 0))),

            Inst.Label(Name.Local("ifafter", 0))

          ), Val.R(r(2, _.U1)))
        }
      }
      "PrefixAp" - {
        compileExpr(ast.PrefixAp(PrefixOp.Neg, ast.IntLit(20) :: I32) :: I32)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Unary(PrefixOp.Neg, 20))
          ), Val.R(r(0, _.I32)))
        compileExpr(ast.PrefixAp(PrefixOp.Not, ast.Var(l('x)) :: U1) :: U1)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.U1), Op.Unary(PrefixOp.Not, r(0, _.U1)))
          ), Val.R(r(1, _.U1)))
      }
      "Widen" - {
        compileExpr(ast.Widen(ast.Var(l('x)) :: I32) :: I64)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.I64), Op.Widen(r(0, _.I32)))
          ), Val.R(r(1, _.I64)))
      }
      "Trim" - {
        compileExpr(ast.Trim(ast.Var(l('x)) :: I64) :: I32)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.I32), Op.Trim(r(0, _.I32)))
          ), Val.R(r(1, _.I32)))
      }
      "Reinterpret" - {
        compileExpr(ast.Reinterpret(ast.Var(l('x)) :: Ptr(TypeName.Named(g('i32)))) :: Ptr(TypeName.Named(g('i64))))
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.Ptr)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.Ptr), Op.Copy(r(0, _.Ptr)))
          ), Val.R(r(1, _.Ptr)))
      }
      "Ignore" - {
        compileExpr(ast.Ignore(ast.App(ast.Var(g('foo)) :: Fun(List(I32), I32), List(ast.IntLit(42) :: I32)) :: I32) :: U0)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Copy(42)),
            Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(List(ir.Type.I32), ir.Type.I32)), List(r(0, _.I32)))),
            Inst.Move(r(2, _.I32), Op.Copy(r(1, _.I32)))
          ), Val.I(0, ir.Type.U0))
      }
      "Var" - {
        "local" - {
          compileExpr(ast.Var(l('x)) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)))).value ==>
            (insts(), Val.R(r(0, _.I32)))
        }
        "global" - {
          compileExpr(ast.Var(g('foo)) :: I32).run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Load(Val.GlobalAddr(Name.Global(List("foo"))), i64(0)))
            ), Val.R(r(0, _.I32)))
        }
      }
      "Block" - {
        compileExpr(ast.Block(Vector(
          ast.ValDef(l('x), None, ast.IntLit(10) :: I32) :: U0,
          ast.ValDef(l('y), None, ast.IntLit(20) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x)) :: I32, ast.Var(l('y)) :: I32) :: I32
        )) :: I32).run.runA(CodegenState()).value ==> (insts(
          Inst.Move(r(0, _.I32), Op.Copy(10)),
          Inst.Move(r(1, _.I32), Op.Copy(20)),
          Inst.Move(r(2, _.I32), Op.Binary(InfixOp.Add, r(0, _.I32), r(1, _.I32)))
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
        "without else branch" - {
          val fooType = Fun(Nil, U0)
          compileExpr(ast.If(ast.Var(l('x)) :: U1,
            ast.App(ast.Var(g('foo)) :: fooType, Nil) :: U0, None) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
            (insts(
              Inst.Flow(FlowControl.Branch(r(0, _.U1), Name.Local("if", 0), Name.Local("ifafter", 0))),

              Inst.Label(Name.Local("if", 0)),
              Inst.Move(r(2, _.U0), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(Nil, ir.Type.U0)), List.empty)),
              Inst.Move(r(3, _.U0), Op.Copy(r(2, _.U0))),
              Inst.Move(r(1, _.U0), Op.Copy(r(3, _.U0))),
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
            Inst.Move(r(1, _.U1), Op.Binary(InfixOp.Lt, r(0, _.I32), 10)),
            Inst.Flow(FlowControl.Branch(r(1, _.U1), Name.Local("while", 0), Name.Local("whileafter", 0))),

            Inst.Label(Name.Local("while", 0)),
            Inst.Move(r(2, _.I32), Op.Binary(InfixOp.Add, r(0, _.I32), 1)),
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
            ), u0)
        }
        "ptr" - {
          compileExpr(ast.Assign(ast.App(ast.Var(l('p)) :: Ptr(TypeName.Named(g('i32))), List(ast.IntLit(8) :: I64)) :: I32,
            None, ast.IntLit(20) :: I32) :: U0).run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, Val.I(8, ir.Type.I64), Val.I(4, ir.Type.I64))),
              Inst.Do(Op.Store(r(0, _.Ptr), r(1, _.I64), 20))
            ), u0)
        }
        "struct member" - {
          val struct = Struct(g('foo), List(
            StructMember("x", I64),
            StructMember("y", I32)
          ))
          val r0 = r(0, _.Struct(Vector(ir.Type.I64, ir.Type.I32)))
          compileExpr(ast.Assign(ast.Select(ast.Var(l('x)) :: struct, "x") :: I64, None, ast.IntLit(20) :: I64) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r0, Op.StructCopy(r0, Map(0 -> Val.I(20, ir.Type.I64))))
            ), u0)
        }
        "arr ptr" - {
          compileExpr(ast.Assign(
            ast.App(
              ast.App(ast.Var(l('foo)) :: Ptr(TypeName.arr(TypeName.Named(g('i32)), 10)), Nil) :: Arr(I32, 10),
              List(ast.IntLit(5) :: I64)) :: I32,
            None, ast.IntLit(42) :: I32
          ) :: U0).run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, i64(5), i64(ir.Type.I32.size))),
              Inst.Do(Op.Store(r(0, _.Ptr), r(1, _.I64), i32(42)))
            ), u0)
        }
      }
      "Select" - {
        "Member" - {
          val fooStruct = Struct(g('foo), List(
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
          compileExpr(ast.App(ast.Var(g('f)) :: Fun(List(I32), U1), List(ast.IntLit(10) :: I32)) :: I32)
            .run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Copy(10)),
              Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("f")), ir.Type.Fun(List(ir.Type.I32), ir.Type.U1)), List(r(0, _.I32)))),
              Inst.Move(r(2, _.I32), Op.Copy(r(1, _.I32)))
            ), Val.R(r(2, _.I32)))
        }
        "pointer" - {
          compileExpr(ast.App(ast.Var(l('p)) :: Ptr(TypeName.Named(g('i32))), Nil) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I32), Op.Load(r(0, _.Ptr), Val.I(0, ir.Type.I64)))
            ), Val.R(r(1, _.I32)))
          "with offset" - {
            compileExpr(ast.App(ast.Var(l('p)) :: Ptr(TypeName.Named(g('i32))), List(ast.IntLit(10) :: I64)) :: I32)
              .run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
              (insts(
                Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, Val.I(10, ir.Type.I64), Val.I(4, ir.Type.I64))),
                Inst.Move(r(2, _.I32), Op.Load(r(0, _.Ptr), r(1, _.I64)))
              ), Val.R(r(2, _.I32)))
          }
        }
        "array pointer" - {
          compileExpr(ast.App(
            ast.App(ast.Var(l('foo)) :: Ptr(TypeName.arr(TypeName.Named(g('i32)), 10)), Nil) :: Arr(I32, 10),
            List(ast.IntLit(5) :: I64)) :: I32).run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, i64(5), i64(ir.Type.I32.size))),
              Inst.Move(r(2, _.I32), Op.Load(r(0, _.Ptr), r(1, _.I64)))
            ), Val.R(r(2, _.I32)))
        }
        "array" - {
          compileExpr(ast.App(ast.Var(l('foo)) :: Arr(I32, 10), List(ast.Var(l('i)) :: I64)) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Arr(ir.Type.I32, 10)), l('i) -> r(1, _.I64)), nextReg = 2)).value ==>
            (insts(
              Inst.Move(r(2, _.I32), Op.Elem(r(0, _.Arr(ir.Type.I32, 10)), r(1, _.I64)))
            ), Val.R(r(2, _.I32)))
        }
      }
      "Stackalloc" - {
        compileExpr(ast.Stackalloc(I64) :: Ptr(TypeName.Named(g('i64))))
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.Ptr), Op.Stackalloc(8))
          ), Val.R(r(0, _.Ptr)))
      }
      "ArrLit" - {
        "list" - {
          compileExpr(ast.ArrLit(List(0, 1, 2, 3, 4).map(ast.IntLit(_) :: I32)) :: Arr(I32, 5))
            .run.runA(CodegenState()).value ==>
            (insts(), Val.Array(List(0, 1, 2, 3, 4).map(i32)))
        }
      }
      "Sqrt" - {
        "f32" - {
          compileExpr(ast.App(ast.Var(g('sqrt)) :: Fun(List(F32), F32), List(ast.Var(l('x)) :: F32)) :: F32)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.F32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.F32), Op.Sqrt(r(0, _.F32)))
            ), Val.R(r(1, _.F32)))
        }
        "f64" - {
          compileExpr(ast.App(ast.Var(g('sqrt)) :: Fun(List(F64), F64), List(ast.Var(l('x)) :: F64)) :: F64)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.F64)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.F64), Op.Sqrt(r(0, _.F64)))
            ), Val.R(r(1, _.F64)))
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
      "ValDef" - {
        "trivial" - {
          "int" - {
            compileDef(ast.ValDef(g('foo), None, ast.IntLit(42) :: I32)) ==>
              Def.TrivialVal(Name.Global(List("foo")), Val.I(42, ir.Type.I32))
          }
          "struct" - {
            val struct = Struct(g('foo), List(StructMember("a", I64)))
            compileDef(ast.ValDef(g('foo), None, ast.StructLit(g('foo), List("a" -> (ast.IntLit(20) :: I64))) :: struct)) ==>
              Def.TrivialVal(Name.Global(List("foo")), Val.Struct(Vector(Val.I(20, ir.Type.I64))))

          }
        }
      }
    }
  }

}
