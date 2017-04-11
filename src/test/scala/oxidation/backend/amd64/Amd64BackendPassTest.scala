package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir._
import Type._
import codegen.{Codegen, Name}
import oxidation.backend.amd64.RegLoc._
import utest._

object Amd64BackendPassTest extends TestSuite with IrValSyntax {

  val pass = Amd64BackendPass

  import Codegen.register

  def br(index: Int, typ: Type): Register = Register(Amd64BackendPass.BackendReg, index, typ)

  val tests = apply {
    "txDef" - {
      "precolours for parameters and return" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")),
          List(register(0, I32), register(1, I32), register(2, I32), register(3, I32), register(4, I32)), I32, Vector(
            Block(Name.Local("body", 0), Vector(
              Inst.Move(register(5, I32), Op.Copy(10))
            ), FlowControl.Return(register(5, I32)))
          ), Set.empty)).written.runA(pass.St()).value ==> Set(
          register(0, I32) -> C,
          register(1, I32) -> D,
          register(2, I32) -> R8,
          register(3, I32) -> R9,
          register(5, I32) -> A
        )
      }
      "precolours for calls returning small structs" - {
        val struct = Type.Struct(Vector(I32, I64, I16, I8, U1))
        val r0 = register(0, struct)
        pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, U0, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(r0, Op.Call(ir.Val.G(Name.Global(List("bar")), Fun(Nil, struct)), Nil)),
            Inst.Move(register(1, I32), Op.Member(r0, 0)),
            Inst.Move(register(2, I64), Op.Member(r0, 1)),
            Inst.Move(register(3, I16), Op.Member(r0, 2)),
            Inst.Move(register(4, I8), Op.Member(r0, 3)),
            Inst.Move(register(5, U1), Op.Member(r0, 4))
          ), FlowControl.Return(ir.Val.I(0, U0)))
        ), Set.empty)).written.runA(pass.St()).value ==> Set(
          register(1, I32) -> A,
          register(2, I64) -> D,
          register(3, I16) -> C,
          register(4, I8) -> R8,
          register(5, U1) -> R9
        )
      }
      "div" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), List(register(0, I32), register(1, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32))),
            Inst.Move(register(3, I32), Op.Copy(register(1, I32))),
            Inst.Move(register(4, I32), Op.Binary(InfixOp.Div, register(2, I32), register(3, I32))),
            Inst.Move(register(5, I32), Op.Copy(register(4, I32)))
          ), FlowControl.Return(register(5, I32)))
        ), Set.empty)).run.runA(pass.St()).value ==> (Set(
          register(0, I32) -> C,
          register(1, I32) -> D,
          br(0, I32) -> A,
          br(1, I32) -> D,
          br(3, I32) -> A,
          br(4, I32) -> D,
          register(5, I32) -> A
        ), Vector(Def.Fun(Name.Global(List("foo")), List(register(0, I32), register(1, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32))),
            Inst.Move(register(3, I32), Op.Copy(register(1, I32))),

            Inst.Move(br(0, I32), Op.Copy(register(2, I32))),
            Inst.Move(br(1, I32), Op.Copy(0)),
            Inst.Move(br(2, I32), Op.Copy(register(3, I32))),
            Inst.Do(Op.Copy(br(1, I32))),
            Inst.Move(br(3, I32), Op.Binary(InfixOp.Div, br(0, I32), br(2, I32))),
            Inst.Move(br(4, I32), Op.Garbled),
            Inst.Move(register(4, I32), Op.Copy(br(3, I32))),

            Inst.Move(register(5, I32), Op.Copy(register(4, I32)))
          ), FlowControl.Return(register(5, I32)))
        ), Set.empty)))
      }
      "return of small structs" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, Struct(Vector(I8, I16, I32, I64, U64)), Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(0, I8), Op.Copy(ir.Val.I(0, I8))),
            Inst.Move(register(1, I16), Op.Copy(ir.Val.I(0, I16))),
            Inst.Move(register(2, I32), Op.Copy(ir.Val.I(0, I32))),
            Inst.Move(register(3, I64), Op.Copy(ir.Val.I(0, I64))),
            Inst.Move(register(4, U64), Op.Copy(ir.Val.I(0, U64)))
          ), FlowControl.Return(ir.Val.Struct(Vector(register(0, I8), register(1, I16), register(2, I32), register(3, I64), register(4, U64)))))
        ), Set.empty)).written.runA(pass.St()).value ==> Set(
          register(0, I8) -> A,
          register(1, I16) -> D,
          register(2, I32) -> C,
          register(3, I64) -> R8,
          register(4, U64) -> R9
        )
      }
    }
  }

}
