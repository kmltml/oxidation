package oxidation
package codegen.pass

import utest._
import ir._
import Type._
import oxidation.codegen.{Name, Codegen}

object StructLoweringTests extends TestSuite with IrValSyntax {

  val pass = StructLowering

  import Codegen.register
  import pass.{register => sl}

  val tests = apply {
    "struct parameters" - {
      val r0 = register(0, Struct(Vector(I32, I64)))
      pass.txDef(Def.Fun(Name.Global(List("foo")), List(r0), I64, Vector.empty, Set.empty))
        .run(pass.S()).value ==>
        (pass.S(nextReg = 2, bindings = Map(r0 -> Vector(sl(0, I32), sl(1, I64)))),
        Vector(
          Def.Fun(Name.Global(List("foo")), List(sl(0, I32), sl(1, I64)), I64, Vector.empty, Set.empty)
        ))
    }
    "struct register write and read" - {
      val r0 = register(0, Struct(Vector(I32, U8)))
      pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
        Block(Name.Local("body", 0), Vector(
          Inst.Move(r0, Op.Copy(Val.Struct(Vector(i32(0), u8(0))))),
          Inst.Move(register(1, I32), Op.Member(r0, 0)),
          Inst.Move(register(2, U8), Op.Member(r0, 1))
        ), FlowControl.Return(u0))
      ), Set.empty)).runA(pass.S()).value ==> Vector(
        Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(sl(0, I32), Op.Copy(i32(0))),
            Inst.Move(sl(1, U8), Op.Copy(u8(0))),
            Inst.Move(register(1, I32), Op.Copy(sl(0, I32))),
            Inst.Move(register(2, U8), Op.Copy(sl(1, U8)))
          ), FlowControl.Return(u0))
        ), Set.empty)
      )
    }
    "call taking struct parameter" - {
      val struct = Struct(Vector(I32, I64))
      val r0 = register(0, struct)
      pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
        Block(Name.Local("body", 0), Vector(
          Inst.Move(r0, Op.Copy(Val.Struct(Vector(i32(10), i64(20))))),
          Inst.Do(Op.Call(Val.G(Name.Global(List("bar")), Fun(List(struct), U0)), List(r0)))
        ), FlowControl.Return(u0))
      ), Set.empty)).runA(pass.S()).value ==> Vector(
        Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(sl(0, I32), Op.Copy(i32(10))),
            Inst.Move(sl(1, I64), Op.Copy(i64(20))),
            Inst.Do(Op.Call(Val.G(Name.Global(List("bar")), Fun(List(struct), U0)), List(sl(0, I32), sl(1, I64))))
          ), FlowControl.Return(u0))
        ), Set.empty)
      )
    }
    "call returning a struct" - {
      val struct = Struct(Vector(I32, I64))
      val r0 = register(0, struct)
      pass.txInstruction(Inst.Move(r0, Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, struct)), Nil)))
        .run(pass.S()).value ==>
        (pass.S(
          nextReg = 3,
          bindings = Map(
            r0 -> Vector(sl(1, I32), sl(2, I64)),
            sl(0, struct) -> Vector(sl(1, I32), sl(2, I64))
          )
        ), Vector(
          Inst.Move(sl(0, struct), Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, struct)), Nil)),
          Inst.Move(sl(1, I32), Op.Member(sl(0, struct), 0)),
          Inst.Move(sl(2, I64), Op.Member(sl(0, struct), 1))
        ))
    }
    "struct copy" - {
      val struct = Struct(Vector(I32, I64))
      val r0 = register(0, struct)
      val r1 = register(1, struct)
      pass.txInstruction(Inst.Move(r1, Op.Copy(r0))).run(pass.S(nextReg = 2, bindings = Map(r0 -> Vector(sl(0, I32), sl(1, I64))))).value ==>
        (pass.S(nextReg = 4, bindings = Map(r0 -> Vector(sl(0, I32), sl(1, I64)), r1 -> Vector(sl(2, I32), sl(3, I64)))), Vector(
          Inst.Move(sl(2, I32), Op.Copy(sl(0, I32))),
          Inst.Move(sl(3, I64), Op.Copy(sl(1, I64)))
        ))
    }
    "struct modify" - {
      val struct = Struct(Vector(I64, I32))
      val r0 = register(0, struct)
      val state = pass.S(bindings = Map(r0 -> Vector(sl(0, I64), sl(1, I32))), nextReg = 2)
      pass.txInstruction(Inst.Move(r0, Op.StructCopy(r0, Map(0 -> i64(10))))).run(state).value ==>
        (state, Vector(
          Inst.Move(sl(0, I64), Op.Copy(i64(10))),
          Inst.Move(sl(1, I32), Op.Copy(sl(1, I32)))
        ))
    }
    "struct load" - {
      val struct = Struct(Vector(I64, I32))
      pass.txInstruction(Inst.Move(register(1, struct), Op.Load(register(0, Ptr), i64(0))))
        .run(pass.S()).value ==>
        (pass.S(nextReg = 3, bindings = Map(register(1, struct) -> Vector(sl(1, I64), sl(2, I32)))), Vector(
          Inst.Move(sl(0, Ptr), Op.Binary(InfixOp.Add, register(0, Ptr), i64(0))),
          Inst.Move(sl(1, I64), Op.Load(sl(0, Ptr), i64(0))),
          Inst.Move(sl(2, I32), Op.Load(sl(0, Ptr), i64(8)))
        ))
    }

    "struct store" - {
      val struct = Struct(Vector(I64, I32))
      val state = pass.S(bindings = Map(register(1, struct) -> Vector(sl(0, I64), sl(1, I32))), nextReg = 2)
      pass.txInstruction(Inst.Do(Op.Store(register(0, Ptr), i64(0), register(1, struct))))
        .run(state).value ==>
        (state.copy(nextReg = 3), Vector(
          Inst.Move(sl(2, Ptr), Op.Binary(InfixOp.Add, register(0, Ptr), i64(0))),
          Inst.Do(Op.Store(sl(2, Ptr), i64(0), sl(0, I64))),
          Inst.Do(Op.Store(sl(2, Ptr), i64(8), sl(1, I32)))
        ))
    }

    "struct equality" - {
      val struct = Struct(Vector(I64, I32))
      val state = pass.S(
        bindings = Map(
          register(0, struct) -> Vector(sl(0, I64), sl(1, I32)),
          register(1, struct) -> Vector(sl(2, I64), sl(3, I32))),
        nextReg = 4)
      pass.txInstruction(Inst.Move(register(2, U1), Op.Binary(InfixOp.Eq, register(0, struct), register(1, struct))))
        .run(state).value ==>
        (state.copy(nextReg = 7), Vector(
          Inst.Move(sl(4, U1), Op.Binary(InfixOp.Eq, sl(0, I64), sl(2, I64))),
          Inst.Move(sl(5, U1), Op.Binary(InfixOp.Eq, sl(1, I32), sl(3, I32))),
          Inst.Move(sl(6, U1), Op.Binary(InfixOp.BitAnd, sl(4, U1), sl(5, U1))),
          Inst.Move(register(2, U1), Op.Copy(sl(6, U1)))
        ))
    }

    "call returning a nested struct" - {
      val foo = Struct(Vector(I32, I32))
      val bar = Struct(Vector(foo, foo))
      val flatbar = Struct(Vector(I32, I32, I32, I32))
      pass.txInstruction(Inst.Move(register(0, bar), Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, bar)), Nil)))
        .run(pass.S()).value ==>
        (pass.S(
          bindings = Map(
            register(0, bar) -> Vector(sl(7, foo), sl(8, foo)),
            sl(0, flatbar) -> Vector(sl(3, I32), sl(4, I32), sl(5, I32), sl(6, I32)),
            sl(1, foo) -> Vector(sl(3, I32), sl(4, I32)),
            sl(2, foo) -> Vector(sl(5, I32), sl(6, I32)),
            sl(7, foo) -> Vector(sl(3, I32), sl(4, I32)),
            sl(8, foo) -> Vector(sl(5, I32), sl(6, I32))
          ),
          nextReg = 9
        ), Vector(
          Inst.Move(sl(0, flatbar), Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, bar)), Nil)),
          Inst.Move(sl(3, I32), Op.Member(sl(0, flatbar), 0)),
          Inst.Move(sl(4, I32), Op.Member(sl(0, flatbar), 1)),
          Inst.Move(sl(5, I32), Op.Member(sl(0, flatbar), 2)),
          Inst.Move(sl(6, I32), Op.Member(sl(0, flatbar), 3))
        ))
    }
  }

}
