package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._

import ir._
import Type._
import codegen.{Codegen, Name}
import oxidation.backend.amd64.Reg._
import utest._

object Amd64BackendPassTest extends TestSuite {

  val pass = Amd64BackendPass

  private implicit def vali(i: Int): ir.Val = ir.Val.I(i, I32)
  private implicit def valr(r: Register): ir.Val = ir.Val.R(r)

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
          )
        )).written.runEmptyA.value ==> Set(
          register(0, I32) -> RCX,
          register(1, I32) -> RDX,
          register(2, I32) -> R8,
          register(3, I32) -> R9,
          register(5, I32) -> RAX
        )
      }
      "div" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), List(register(0, I32), register(1, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32))),
            Inst.Move(register(3, I32), Op.Copy(register(1, I32))),
            Inst.Move(register(4, I32), Op.Arith(InfixOp.Div, register(2, I32), register(3, I32))),
            Inst.Move(register(5, I32), Op.Copy(register(4, I32)))
          ), FlowControl.Return(register(5, I32)))
        ))).run.runEmptyA.value ==> (Set(
          register(0, I32) -> RCX,
          register(1, I32) -> RDX,
          br(0, I32) -> RAX,
          br(1, I32) -> RDX,
          br(3, I32) -> RAX,
          br(4, I32) -> RDX,
          register(5, I32) -> RAX
        ), Vector(Def.Fun(Name.Global(List("foo")), List(register(0, I32), register(1, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32))),
            Inst.Move(register(3, I32), Op.Copy(register(1, I32))),

            Inst.Move(br(0, I32), Op.Copy(register(2, I32))),
            Inst.Move(br(1, I32), Op.Copy(0)),
            Inst.Move(br(2, I32), Op.Copy(register(3, I32))),
            Inst.Do(Op.Copy(br(1, I32))),
            Inst.Move(br(3, I32), Op.Arith(InfixOp.Div, br(0, I32), br(2, I32))),
            Inst.Move(br(4, I32), Op.Garbled),
            Inst.Move(register(4, I32), Op.Copy(br(3, I32))),

            Inst.Move(register(5, I32), Op.Copy(register(4, I32)))
          ), FlowControl.Return(register(5, I32)))
        ))))
      }
    }
  }

}
