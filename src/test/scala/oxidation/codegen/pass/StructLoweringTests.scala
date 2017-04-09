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
      val r0 = register(0, Struct(Vector(I32, I64)))
      pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
        Block(Name.Local("body", 0), Vector(
          Inst.Move(r0, Op.Copy(Val.Struct(Vector(Val.I(0, I32), Val.I(0, U8))))),
          Inst.Move(register(1, I32), Op.Member(r0, 0)),
          Inst.Move(register(2, U8), Op.Member(r0, 1))
        ), FlowControl.Return(Val.I(0, U0)))
      ), Set.empty)).runA(pass.S()).value ==> Vector(
        Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(sl(0, I32), Op.Copy(Val.I(0, I32))),
            Inst.Move(sl(1, U8), Op.Copy(Val.I(0, U8))),
            Inst.Move(register(1, I32), Op.Copy(sl(0, I32))),
            Inst.Move(register(2, U8), Op.Copy(sl(1, U8)))
          ), FlowControl.Return(Val.I(0, U0)))
        ), Set.empty)
      )
    }
    "call taking struct parameter" - {
      val struct = Struct(Vector(I32, I64))
      val r0 = register(0, struct)
      pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
        Block(Name.Local("body", 0), Vector(
          Inst.Move(r0, Op.Copy(Val.Struct(Vector(Val.I(10, I32), Val.I(20, I64))))),
          Inst.Do(Op.Call(Val.G(Name.Global(List("bar")), Fun(List(struct), U0)), List(r0)))
        ), FlowControl.Return(Val.I(0, U0)))
      ), Set.empty)).runA(pass.S()).value ==> Vector(
        Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(sl(0, I32), Op.Copy(Val.I(10, I32))),
            Inst.Move(sl(1, I64), Op.Copy(Val.I(20, I64))),
            Inst.Do(Op.Call(Val.G(Name.Global(List("bar")), Fun(List(struct), U0)), List(sl(0, I32), sl(1, I64))))
          ), FlowControl.Return(Val.I(0, U0)))
        ), Set.empty)
      )
    }
    "call returning a struct" - {
      val struct = Struct(Vector(I32, I64))
      val r0 = register(0, struct)
      pass.txInstruction(Inst.Move(r0, Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, struct)), Nil)))
        .run(pass.S()).value ==>
        (pass.S(
          nextReg = 2,
          bindings = Map(
            r0 -> Vector(sl(0, I32), sl(1, I64))
          )
        ), Vector(
          Inst.Move(r0, Op.Call(Val.G(Name.Global(List("foo")), Fun(Nil, struct)), Nil)),
          Inst.Move(sl(0, I32), Op.Member(r0, 0)),
          Inst.Move(sl(1, I64), Op.Member(r0, 1))
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
  }

}
