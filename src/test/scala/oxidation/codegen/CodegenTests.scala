package oxidation
package codegen

import analyze.{BuiltinSymbols, TypedSyntax, ast}
import analyze.Type._
import ir._
import oxidation.parse.Span
import utest._

object CodegenTests extends TestSuite with TypedSyntax with SymbolSyntax with IrValSyntax with MatchCaseSyntax {

  import Codegen.register

  private def r(i: Int, t: ir.Type.type => ir.Type): Register = register(i, t(ir.Type))

  private def insts(insts: Inst*): Codegen.Log = Codegen.Log(insts.toVector)

  val loc = Span(None, 0, 0)

  val tests = apply {
    "compileExpr" - {
      import Codegen.compileExpr
      "IntLit" - {
        compileExpr(ast.IntLit(20, loc) :: I32)
          .run.runA(CodegenState()).value ==> (insts(), Val.I(20, ir.Type.I32))
      }
      "FloatLit" - {
        "F32" - {
          compileExpr(ast.FloatLit(BigDecimal(0.1f), loc) :: F32)
            .run.runA(CodegenState()).value ==> (insts(), Val.F32(0.1f))
        }
        "F64" - {
          compileExpr(ast.FloatLit(BigDecimal(0.1), loc) :: F64)
            .run.runA(CodegenState()).value ==> (insts(), Val.F64(0.1))
        }
      }
      "BoolLit" - {
        compileExpr(ast.BoolLit(true, loc) :: U1).run.runA(CodegenState()).value ==>
          (insts(), Val.I(1, ir.Type.U1))
        compileExpr(ast.BoolLit(false, loc) :: U1).run.runA(CodegenState()).value ==>
          (insts(), Val.I(0, ir.Type.U1))
      }
      "CharLit" - {
        compileExpr(ast.CharLit('a', loc) :: U8).run.runA(CodegenState()).value ==>
          (insts(), Val.I(97, ir.Type.U8))
      }
      "StructLit" - {
        compileExpr(ast.StructLit(g('str), List(
          "data" -> (ast.Var(l('x), loc) :: Ptr(U8)),
          "length" -> (ast.IntLit(10, loc) :: U32)
        ), loc) :: BuiltinSymbols.StrType).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.Ptr)), nextReg = 1)).value ==>
          (insts(), Val.Struct(Vector(r(0, _.Ptr), Val.I(10, ir.Type.U32))))
      }
      "EnumLit" - {
        val option = Enum(g('Option), List(
          EnumVariant(g('Option, 'Some), List(StructMember("value", I32))),
          EnumVariant(g('Option, 'None), Nil)
        ))
        compileExpr(ast.EnumLit("Some", List(
          "value" -> (ast.IntLit(10, loc) :: I32)
        ), loc) :: option).run.runA(CodegenState()).value ==>
        (insts(), Val.Enum(0, Vector(i32(10)), ir.Type.Enum(List(ir.Type.Struct(Vector(ir.Type.I32)), ir.Type.Struct(Vector.empty)))))
      }
      "StringLit" - {
        compileExpr(ast.StringLit("Hello, world!", loc) :: BuiltinSymbols.StrType).run.runA(CodegenState()).value ==>
          (Codegen.Log(Vector.empty, Vector(ConstantPoolEntry.Str("Hello, world!"))),
            Val.Struct(Vector(Val.Const(ConstantPoolEntry.Str("Hello, world!"), ir.Type.Ptr), Val.I(13, ir.Type.U32))))
      }
      "UnitLit" - {
        compileExpr(ast.UnitLit(loc) :: U0).run.runA(CodegenState()).value ==>
          (Codegen.Log(Vector.empty), Val.I(0, ir.Type.U0))
      }
      "InfixAp" - {
        compileExpr(ast.InfixAp(InfixOp.Add, ast.IntLit(1, loc) :: I32, ast.IntLit(2, loc) :: I32, loc) :: I32)
          .run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.I32), Op.Binary(InfixOp.Add, 1, 2))
          ), Val.R(r(0, _.I32)))

        compileExpr(ast.InfixAp(InfixOp.Add,
          ast.InfixAp(InfixOp.Sub, ast.IntLit(3, loc) :: I32, ast.IntLit(2, loc) :: I32, loc) :: I32,
          ast.IntLit(1, loc) :: I32, loc) :: I32).run.runA(CodegenState()).value ==> (insts(
            Inst.Move(r(0, _.I32), Op.Binary(InfixOp.Sub, 3, 2)),
            Inst.Move(r(1, _.I32), Op.Binary(InfixOp.Add, r(0, _.I32), 1))
          ), Val.R(r(1, _.I32)))
        "Or" - {
          compileExpr(ast.InfixAp(InfixOp.Or,
            ast.App(ast.Var(g('foo), loc) :: Fun(Nil, U1), Nil, loc) :: U1,
            ast.App(ast.Var(g('bar), loc) :: Fun(Nil, U1), Nil, loc) :: U1, loc
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
            ast.App(ast.Var(g('foo), loc) :: Fun(Nil, U1), Nil, loc) :: U1,
            ast.App(ast.Var(g('bar), loc) :: Fun(Nil, U1), Nil, loc) :: U1, loc
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
        compileExpr(ast.PrefixAp(PrefixOp.Neg, ast.IntLit(20, loc) :: I32, loc) :: I32)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Unary(PrefixOp.Neg, 20))
          ), Val.R(r(0, _.I32)))
        compileExpr(ast.PrefixAp(PrefixOp.Not, ast.Var(l('x), loc) :: U1, loc) :: U1)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.U1)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.U1), Op.Unary(PrefixOp.Not, r(0, _.U1)))
          ), Val.R(r(1, _.U1)))
      }
      "Widen" - {
        compileExpr(ast.Widen(ast.Var(l('x), loc) :: I32, loc) :: I64)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.I64), Op.Widen(r(0, _.I32)))
          ), Val.R(r(1, _.I64)))
      }
      "Trim" - {
        compileExpr(ast.Trim(ast.Var(l('x), loc) :: I64, loc) :: I32)
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.I32), Op.Trim(r(0, _.I32)))
          ), Val.R(r(1, _.I32)))
      }
      "Reinterpret" - {
        compileExpr(ast.Reinterpret(ast.Var(l('x), loc) :: Ptr(I32), loc) :: Ptr(I64))
          .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.Ptr)), nextReg = 1)).value ==>
          (insts(
            Inst.Move(r(1, _.Ptr), Op.Copy(r(0, _.Ptr)))
          ), Val.R(r(1, _.Ptr)))
      }
      "Convert" - {
        "f32 -> i64" - {
          compileExpr(ast.Convert(ast.Var(l('x), loc) :: F32, loc) :: I64)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.F32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Convert(r(0, _.F32), ir.Type.I64))
            ), Val.R(r(1, _.I64)))
        }
        "f64 -> i32" - {
          compileExpr(ast.Convert(ast.Var(l('x), loc) :: F64, loc) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.F64)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I32), Op.Convert(r(0, _.F64), ir.Type.I32))
            ), Val.R(r(1, _.I32)))
        }
        "global i32 => u1  ->  funptr[i32 => u1]" - {
          compileExpr(ast.Convert(ast.Var(g('foo), loc) :: Fun(List(I32), U1), loc) :: FunPtr(List(I32), U1))
            .run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.Ptr), Op.Copy(Val.GlobalAddr(Name.Global(List("foo")))))
            ), Val.R(r(0, _.Ptr)))
        }
      }
      "Ignore" - {
        compileExpr(ast.Ignore(ast.App(ast.Var(g('foo), loc) :: Fun(List(I32), I32), List(ast.IntLit(42, loc) :: I32), loc) :: I32, loc) :: U0)
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.I32), Op.Copy(42)),
            Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("foo")), ir.Type.Fun(List(ir.Type.I32), ir.Type.I32)), List(r(0, _.I32)))),
            Inst.Move(r(2, _.I32), Op.Copy(r(1, _.I32)))
          ), Val.I(0, ir.Type.U0))
      }
      "Var" - {
        "local" - {
          compileExpr(ast.Var(l('x), loc) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)))).value ==>
            (insts(), Val.R(r(0, _.I32)))
        }
        "global" - {
          compileExpr(ast.Var(g('foo), loc) :: I32).run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Load(Val.GlobalAddr(Name.Global(List("foo"))), i64(0)))
            ), Val.R(r(0, _.I32)))
        }
      }
      "Block" - {
        compileExpr(ast.Block(Vector(
          ast.ValDef(l('x), None, ast.IntLit(10, loc) :: I32) :: U0,
          ast.ValDef(l('y), None, ast.IntLit(20, loc) :: I32) :: U0,
          ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.Var(l('y), loc) :: I32, loc) :: I32
        ), loc) :: I32).run.runA(CodegenState()).value ==> (insts(
          Inst.Move(r(0, _.I32), Op.Copy(10)),
          Inst.Move(r(1, _.I32), Op.Copy(20)),
          Inst.Move(r(2, _.I32), Op.Binary(InfixOp.Add, r(0, _.I32), r(1, _.I32)))
        ), Val.R(r(2, _.I32)))
      }
      "If" - {
        "with else branch" - {
          compileExpr(ast.If(ast.Var(l('x), loc) :: U1, ast.IntLit(10, loc) :: I32, Some(ast.IntLit(20, loc) :: I32), loc) :: I32)
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
          compileExpr(ast.If(ast.Var(l('x), loc) :: U1,
            ast.App(ast.Var(g('foo), loc) :: fooType, Nil, loc) :: U0, None, loc) :: U0)
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
          ast.InfixAp(InfixOp.Lt, ast.Var(l('x), loc) :: I32, ast.IntLit(10, loc) :: I32, loc) :: U1,
          ast.Assign(ast.Var(l('x), loc) :: I32, None,
            ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.IntLit(1, loc) :: I32, loc) :: I32, loc) :: U0, loc
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
      "Match" - {
        "i32" - {
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: I32,
            List(
              (ast.Pattern.IntLit(0, loc) :: I32) -> (ast.IntLit(20, loc) :: I32),
              (ast.Pattern.IntLit(1, loc) :: I32) -> (ast.IntLit(30, loc) :: I32),
              (ast.Pattern.IntLit(2, loc) :: I32) -> (ast.IntLit(40, loc) :: I32),
              (ast.Pattern.Var(l('y), loc) :: I32) -> (ast.Var(l('y), loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(2, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(0))),
              Inst.Flow(FlowControl.Branch(r(2, _.U1), Name.Local("case", 1), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(20))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(1))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("case", 2), Name.Local("casenext", 2))),

              Inst.Label(Name.Local("case", 2)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(30))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 2)),
              Inst.Move(r(4, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(2))),
              Inst.Flow(FlowControl.Branch(r(4, _.U1), Name.Local("case", 3), Name.Local("casenext", 3))),

              Inst.Label(Name.Local("case", 3)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(40))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 3)),
              Inst.Move(r(5, _.I32), Op.Copy(r(0, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 4))),

              Inst.Label(Name.Local("case", 4)),
              Inst.Move(r(1, _.I32), Op.Copy(r(5, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 4)),
              Inst.Label(Name.Local("matchafter", 0))

            ), Val.R(r(1, _.I32)))
        }
        "struct" - {
          val struct = Struct(g('foo), StructMember("x", I32), StructMember("y", I32))
          val irstruct = Type.Struct(Vector(Type.I32, Type.I32))
          val r0 = register(0, irstruct)
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: struct, List(
              (ast.Pattern.Struct(None, List(
                "x" -> (ast.Pattern.IntLit(1, loc) :: I32),
                "y" -> (ast.Pattern.Var(l('y), loc) :: I32)
              ), ignoreExtra = false, loc) :: struct) -> (ast.Var(l('y), loc) :: I32),
              (ast.Pattern.Struct(None, List(
                "x" -> (ast.Pattern.Var(l('x), loc) :: I32),
                "y" -> (ast.Pattern.Var(l('y), loc) :: I32)
              ), ignoreExtra = false, loc) :: struct) -> (ast.InfixAp(InfixOp.Add, ast.Var(l('x), loc) :: I32, ast.Var(l('y), loc) :: I32, loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(2, _.I32), Op.Member(r0, 0)),
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(2, _.I32), i32(1))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("struct", 2), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("struct", 2)),
              Inst.Move(r(4, _.I32), Op.Member(r0, 1)),
              Inst.Move(r(5, _.I32), Op.Copy(r(4, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(1, _.I32), Op.Copy(r(5, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Move(r(6, _.I32), Op.Member(r0, 0)),
              Inst.Move(r(7, _.I32), Op.Copy(r(6, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("struct", 4))),

              Inst.Label(Name.Local("struct", 4)),
              Inst.Move(r(8, _.I32), Op.Member(r0, 1)),
              Inst.Move(r(9, _.I32), Op.Copy(r(8, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 3))),

              Inst.Label(Name.Local("case", 3)),
              Inst.Move(r(10, _.I32), Op.Binary(InfixOp.Add, r(7, _.I32), r(9, _.I32))),
              Inst.Move(r(1, _.I32), Op.Copy(r(10, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 3)),
              Inst.Label(Name.Local("matchafter", 0))

            ), Val.R(r(1, _.I32)))
        }
        "enum" - {
          val option = Enum(g('Option), List(
            EnumVariant(g('Option, 'Some), List(StructMember("value", I32))),
            EnumVariant(g('Option, 'None), Nil)
          ))
          val irSome = ir.Type.Struct(Vector(ir.Type.I32))
          val irNone = ir.Type.Struct(Vector.empty)
          val irOption = ir.Type.Enum(List(irSome, irNone))
          val r0 = register(0, irOption)
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: option, List(
              (ast.Pattern.Enum(g('Option, 'Some), List(
                "value" -> (ast.Pattern.Var(l('y), loc) :: I32)
              ), ignoreExtra = false, loc) :: option) -> (ast.Var(l('y), loc) :: I32),
              (ast.Pattern.Enum(g('Option, 'None), Nil, ignoreExtra = false, loc) :: option) ->
                (ast.IntLit(0, loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(2, _.U8), Op.TagOf(r0)),
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(2, _.U8), u8(0))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("enum", 2), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("enum", 2)),
              Inst.Move(register(4, irSome), Op.Unpack(r0, 0)),
              Inst.Move(r(5, _.I32), Op.Member(register(4, irSome), 0)),
              Inst.Move(r(6, _.I32), Op.Copy(r(5, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(1, _.I32), Op.Copy(r(6, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Move(r(7, _.U8), Op.TagOf(r0)),
              Inst.Move(r(8, _.U1), Op.Binary(InfixOp.Eq, r(7, _.U8), u8(1))),
              Inst.Flow(FlowControl.Branch(r(8, _.U1), Name.Local("enum", 4), Name.Local("casenext", 3))),

              Inst.Label(Name.Local("enum", 4)),
              Inst.Move(register(9, irNone), Op.Unpack(r0, 1)),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 3))),

              Inst.Label(Name.Local("case", 3)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(0))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 3)),
              Inst.Label(Name.Local("matchafter", 0))
            ), Val.R(r(1, _.I32)))
        }
        "Or" - {
          "i32" - {
            compileExpr(ast.Match(
              ast.Var(l('x), loc) :: I32, List(
                (ast.Pattern.Or(
                  ast.Pattern.IntLit(0, loc) :: I32,
                  ast.Pattern.IntLit(1, loc) :: I32,
                  loc
                ) :: I32) -> (ast.IntLit(1, loc) :: I32),
                (ast.Pattern.Var(l('x), loc) :: I32) -> (ast.Var(l('x), loc) :: I32)
              ), loc
            ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
              (insts(
                Inst.Move(r(2, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(0))),
                Inst.Flow(FlowControl.Branch(r(2, _.U1), Name.Local("case", 1), Name.Local("orpattern", 2))),

                Inst.Label(Name.Local("orpattern", 2)),
                Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(1))),
                Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("case", 1), Name.Local("casenext", 1))),

                Inst.Label(Name.Local("case", 1)),
                Inst.Move(r(1, _.I32), Op.Copy(i32(1))),
                Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

                Inst.Label(Name.Local("casenext", 1)),
                Inst.Move(r(4, _.I32), Op.Copy(r(0, _.I32))),
                Inst.Flow(FlowControl.Goto(Name.Local("case", 3))),

                Inst.Label(Name.Local("case", 3)),
                Inst.Move(r(1, _.I32), Op.Copy(r(4, _.I32))),
                Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

                Inst.Label(Name.Local("casenext", 3)),
                Inst.Label(Name.Local("matchafter", 0))

              ), Val.R(r(1, _.I32)))
          }
          "{u1, i32, i32} with bindings" - {
            val struct = Struct(g('foo), StructMember("b", U1), StructMember("x", I32), StructMember("y", I32))
            val irstruct = ir.Type.Struct(Vector(ir.Type.U1, ir.Type.I32, ir.Type.I32))
            val r0 = register(0, irstruct)
            compileExpr(ast.Match(
              ast.Var(l('x), loc) :: struct, List(
                (ast.Pattern.Or(
                  ast.Pattern.Struct(None, List(
                    "b" -> (ast.Pattern.BoolLit(true, loc) :: U1),
                    "x" -> (ast.Pattern.Var(l('x), loc) :: I32)
                  ), ignoreExtra = true, loc) :: struct,
                  ast.Pattern.Struct(None, List(
                    "b" -> (ast.Pattern.BoolLit(false, loc) :: U1),
                    "y" -> (ast.Pattern.Var(l('x), loc) :: I32)
                  ), ignoreExtra = true, loc) :: struct,
                  loc
                ) :: struct) -> (ast.Var(l('x), loc) :: I32)
              ), loc
            ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
              (insts(
                Inst.Move(r(2, _.U1), Op.Member(r0, 0)),
                Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(2, _.U1), u1(true))),
                Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("struct", 3), Name.Local("orpattern", 2))),

                Inst.Label(Name.Local("struct", 3)),
                Inst.Move(r(4, _.I32), Op.Member(r0, 1)),
                Inst.Move(r(5, _.I32), Op.Copy(r(4, _.I32))),
                Inst.Flow(FlowControl.Goto(Name.Local("case", 1))),

                Inst.Label(Name.Local("orpattern", 2)),
                Inst.Move(r(6, _.U1), Op.Member(r0, 0)),
                Inst.Move(r(7, _.U1), Op.Binary(InfixOp.Eq, r(6, _.U1), u1(false))),
                Inst.Flow(FlowControl.Branch(r(7, _.U1), Name.Local("struct", 4), Name.Local("casenext", 1))),

                Inst.Label(Name.Local("struct", 4)),
                Inst.Move(r(8, _.I32), Op.Member(r0, 2)),
                Inst.Move(r(5, _.I32), Op.Copy(r(8, _.I32))),
                Inst.Flow(FlowControl.Goto(Name.Local("case", 1))),

                Inst.Label(Name.Local("case", 1)),
                Inst.Move(r(1, _.I32), Op.Copy(r(5, _.I32))),
                Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

                Inst.Label(Name.Local("casenext", 1)),
                Inst.Label(Name.Local("matchafter", 0))

              ), Val.R(r(1, _.I32)))
          }
        }
        "Alias" - {
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: I32, List(
              (ast.Pattern.Alias(l('y), ast.Pattern.IntLit(10, loc) :: I32, loc) :: I32) ->
                (ast.Var(l('y), loc) :: I32),
              (ast.Pattern.Ignore(loc) :: I32) -> (ast.IntLit(10, loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(2, _.I32), Op.Copy(r(0, _.I32))),
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), i32(10))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("case", 1), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(1, _.I32), Op.Copy(r(2, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 2))),

              Inst.Label(Name.Local("case", 2)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(10))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 2)),
              Inst.Label(Name.Local("matchafter", 0))

            ), Val.R(r(1, _.I32)))
        }
        "guard" - {
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: I32, List(
              ast.MatchCase(ast.Pattern.Var(l('y), loc) :: I32,
                guard = Some(ast.InfixAp(InfixOp.Leq, ast.Var(l('y), loc) :: I32, ast.IntLit(10, loc) :: I32, loc) :: U1),
                ast.Var(l('y), loc) :: I32),
              (ast.Pattern.Ignore(loc) :: I32) -> (ast.IntLit(10, loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(2, _.I32), Op.Copy(r(0, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("guard", 1))),

              Inst.Label(Name.Local("guard", 1)),
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Leq, r(2, _.I32), i32(10))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("case", 1), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(1, _.I32), Op.Copy(r(2, _.I32))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 2))),

              Inst.Label(Name.Local("case", 2)),
              Inst.Move(r(1, _.I32), Op.Copy(i32(10))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 2)),
              Inst.Label(Name.Local("matchafter", 0))
            ), Val.R(r(1, _.I32)))
        }
        "Pin" - {
          compileExpr(ast.Match(
            ast.Var(l('x), loc) :: I32, List(
              (ast.Pattern.Pin(ast.Var(l('y), loc) :: I32, loc) :: I32) -> (ast.IntLit(10, loc) :: I32),
              (ast.Pattern.Ignore(loc) :: I32) -> (ast.IntLit(20, loc) :: I32)
            ), loc
          ) :: I32).run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32), l('y) -> r(1, _.I32)), nextReg = 2)).value ==>
            (insts(
              Inst.Move(r(3, _.U1), Op.Binary(InfixOp.Eq, r(0, _.I32), r(1, _.I32))),
              Inst.Flow(FlowControl.Branch(r(3, _.U1), Name.Local("case", 1), Name.Local("casenext", 1))),

              Inst.Label(Name.Local("case", 1)),
              Inst.Move(r(2, _.I32), Op.Copy(i32(10))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 1)),
              Inst.Flow(FlowControl.Goto(Name.Local("case", 2))),

              Inst.Label(Name.Local("case", 2)),
              Inst.Move(r(2, _.I32), Op.Copy(i32(20))),
              Inst.Flow(FlowControl.Goto(Name.Local("matchafter", 0))),

              Inst.Label(Name.Local("casenext", 2)),
              Inst.Label(Name.Local("matchafter", 0))

            ), Val.R(r(2, _.I32)))
        }
      }
      "Assign" - {
        "variable" - {
          compileExpr(ast.Assign(ast.Var(l('x), loc) :: I32, None, ast.IntLit(20, loc) :: I32, loc) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.I32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Copy(20))
            ), u0)
        }
        "ptr" - {
          compileExpr(ast.Assign(ast.App(ast.Var(l('p), loc) :: Ptr(I32), List(ast.IntLit(8, loc) :: I64), loc) :: I32,
            None, ast.IntLit(20, loc) :: I32, loc) :: U0).run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, Val.I(8, ir.Type.I64), Val.I(4, ir.Type.I64))),
              Inst.Do(Op.Store(r(0, _.Ptr), r(1, _.I64), 20))
            ), u0)
        }
        "struct member" - {
          val struct = Struct(g('foo),
            StructMember("x", I64),
            StructMember("y", I32)
          )
          val r0 = r(0, _.Struct(Vector(ir.Type.I64, ir.Type.I32)))
          compileExpr(ast.Assign(ast.Select(ast.Var(l('x), loc) :: struct, "x", loc) :: I64, None, ast.IntLit(20, loc) :: I64, loc) :: U0)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r0, Op.StructCopy(r0, Map(0 -> Val.I(20, ir.Type.I64))))
            ), u0)
        }
        "arr ptr" - {
          compileExpr(ast.Assign(
            ast.App(
              ast.App(ast.Var(l('foo), loc) :: Ptr(Arr(I32, 10)), Nil, loc) :: Arr(I32, 10),
              List(ast.IntLit(5, loc) :: I64), loc) :: I32,
            None, ast.IntLit(42, loc) :: I32, loc
          ) :: U0).run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, i64(5), i64(ir.Type.I32.size))),
              Inst.Do(Op.Store(r(0, _.Ptr), r(1, _.I64), i32(42)))
            ), u0)
        }
      }
      "Select" - {
        val fooStruct = Struct(g('foo),
          StructMember("x", I32),
          StructMember("y", I64)
        )
        "Member" - {
          val r0 = register(0, ir.Type.Struct(Vector(ir.Type.I32, ir.Type.I64)))
          compileExpr(ast.Select(ast.Var(l('x), loc) :: fooStruct, "y", loc) :: I64)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r0), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Member(r0, 1))
            ), Val.R(r(1, _.I64)))
        }
        "Ptr" - {
          compileExpr(ast.Select(ast.Var(l('x), loc) :: Ptr(fooStruct), "y", loc) :: Ptr(I64))
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.Ptr), Op.Binary(InfixOp.Add, r(0, _.Ptr), i64(4)))
            ), Val.R(r(1, _.Ptr)))
        }
      }
      "App" - {
        "function" - {
          compileExpr(ast.App(ast.Var(g('f), loc) :: Fun(List(I32), I32), List(ast.IntLit(10, loc) :: I32), loc) :: I32)
            .run.runA(CodegenState()).value ==>
            (insts(
              Inst.Move(r(0, _.I32), Op.Copy(10)),
              Inst.Move(r(1, _.I32), Op.Call(Val.G(Name.Global(List("f")), ir.Type.Fun(List(ir.Type.I32), ir.Type.I32)), List(r(0, _.I32)))),
              Inst.Move(r(2, _.I32), Op.Copy(r(1, _.I32)))
            ), Val.R(r(2, _.I32)))
        }
        "FunPtr" - {
          compileExpr(ast.App(ast.Var(l('f), loc) :: FunPtr(List(I32), U1), List(ast.IntLit(10, loc) :: I32), loc) :: U1)
            .run.runA(CodegenState(registerBindings = Map(l('f) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I32), Op.Copy(10)),
              Inst.Move(r(2, _.U1), Op.Call(r(0, _.Ptr), List(r(1, _.I32)))),
              Inst.Move(r(3, _.U1), Op.Copy(r(2, _.U1)))
            ), Val.R(r(3, _.U1)))
        }
        "pointer" - {
          compileExpr(ast.App(ast.Var(l('p), loc) :: Ptr(I32), Nil, loc) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I32), Op.Load(r(0, _.Ptr), Val.I(0, ir.Type.I64)))
            ), Val.R(r(1, _.I32)))
          "with offset" - {
            compileExpr(ast.App(ast.Var(l('p), loc) :: Ptr(I32), List(ast.IntLit(10, loc) :: I64), loc) :: I32)
              .run.runA(CodegenState(registerBindings = Map(l('p) -> r(0, _.Ptr)), nextReg = 1)).value ==>
              (insts(
                Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, Val.I(10, ir.Type.I64), Val.I(4, ir.Type.I64))),
                Inst.Move(r(2, _.I32), Op.Load(r(0, _.Ptr), r(1, _.I64)))
              ), Val.R(r(2, _.I32)))
          }
        }
        "array pointer" - {
          compileExpr(ast.App(
            ast.App(ast.Var(l('foo), loc) :: Ptr(Arr(I32, 10)), Nil, loc) :: Arr(I32, 10),
            List(ast.IntLit(5, loc) :: I64), loc) :: I32).run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Ptr)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.I64), Op.Binary(InfixOp.Mul, i64(5), i64(ir.Type.I32.size))),
              Inst.Move(r(2, _.I32), Op.Load(r(0, _.Ptr), r(1, _.I64)))
            ), Val.R(r(2, _.I32)))
        }
        "array" - {
          compileExpr(ast.App(ast.Var(l('foo), loc) :: Arr(I32, 10), List(ast.Var(l('i), loc) :: I64), loc) :: I32)
            .run.runA(CodegenState(registerBindings = Map(l('foo) -> r(0, _.Arr(ir.Type.I32, 10)), l('i) -> r(1, _.I64)), nextReg = 2)).value ==>
            (insts(
              Inst.Move(r(2, _.I32), Op.Elem(r(0, _.Arr(ir.Type.I32, 10)), r(1, _.I64)))
            ), Val.R(r(2, _.I32)))
        }
      }
      "Stackalloc" - {
        compileExpr(ast.Stackalloc(I64, loc) :: Ptr(I64))
          .run.runA(CodegenState()).value ==>
          (insts(
            Inst.Move(r(0, _.Ptr), Op.Stackalloc(8))
          ), Val.R(r(0, _.Ptr)))
      }
      "ArrLit" - {
        "list" - {
          compileExpr(ast.ArrLit(List(0, 1, 2, 3, 4).map(ast.IntLit(_, loc) :: I32), loc) :: Arr(I32, 5))
            .run.runA(CodegenState()).value ==>
            (insts(), Val.Array(List(0, 1, 2, 3, 4).map(i32)))
        }
      }
      "Sqrt" - {
        "f32" - {
          compileExpr(ast.App(ast.Var(g('sqrt), loc) :: Fun(List(F32), F32), List(ast.Var(l('x), loc) :: F32), loc) :: F32)
            .run.runA(CodegenState(registerBindings = Map(l('x) -> r(0, _.F32)), nextReg = 1)).value ==>
            (insts(
              Inst.Move(r(1, _.F32), Op.Sqrt(r(0, _.F32)))
            ), Val.R(r(1, _.F32)))
        }
        "f64" - {
          compileExpr(ast.App(ast.Var(g('sqrt), loc) :: Fun(List(F64), F64), List(ast.Var(l('x), loc) :: F64), loc) :: F64)
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
          compileDef(ast.DefDef(g('mod, 'foo), Some(List(ast.Param("x", I64))), Some(TypeName.Named(g('i32))), ast.Extern(loc) :: I32)) ==>
            Def.ExternFun(Name.Global(List("mod", "foo")), List(ir.Type.I64), ir.Type.I32)
        }
        "Collects string literals" - {
          val strStruct = ir.Type.Struct(Vector(ir.Type.Ptr, ir.Type.U32))
          compileDef(ast.DefDef(g('foo), None, None, ast.Block(Vector(
            ast.ValDef(l('x), None, ast.StringLit("one", loc) :: BuiltinSymbols.StrType) :: U0,
            ast.ValDef(l('y), None, ast.StringLit("two", loc) :: BuiltinSymbols.StrType) :: U0,
            ast.ValDef(l('z), None, ast.StringLit("two", loc) :: BuiltinSymbols.StrType) :: U0,
            ast.IntLit(10, loc) :: I32
          ), loc) :: I32)) ==>
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
            compileDef(ast.ValDef(g('foo), None, ast.IntLit(42, loc) :: I32)) ==>
              Def.TrivialVal(Name.Global(List("foo")), Val.I(42, ir.Type.I32))
          }
          "struct" - {
            val struct = Struct(g('foo), StructMember("a", I64))
            compileDef(ast.ValDef(g('foo), None, ast.StructLit(g('foo), List("a" -> (ast.IntLit(20, loc) :: I64)), loc) :: struct)) ==>
              Def.TrivialVal(Name.Global(List("foo")), Val.Struct(Vector(Val.I(20, ir.Type.I64))))

          }
        }
      }
    }
  }

}
