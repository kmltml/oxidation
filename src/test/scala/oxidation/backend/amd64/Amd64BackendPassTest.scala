package oxidation
package backend
package amd64

import cats._
import cats.data._
import cats.implicits._
import ir._
import Type._
import codegen.{Codegen, Name}
import RegLoc._
import Xmm._
import utest._

object Amd64BackendPassTest extends TestSuite with IrValSyntax with NameSyntax {

  val pass = Amd64BackendPass

  import pass.Colours
  import Codegen.register

  def br(index: Int, typ: Type): Register = Register(Amd64BackendPass.BackendReg, index, typ)

  val tests = apply {
    "txDef" - {
      "precolours for parameters and return" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")),
          List(register(0, I32), register(1, I32), register(2, I32), register(3, I32), register(4, I32)), I32, Vector(
            Block(Name.Local("body", 0), Vector.empty,
              FlowControl.Return(i32(10)))
          ), Set.empty)).run.runA(pass.St()).value ==>
          (Colours(int = Set(
            br(0, I32) -> C,
            br(1, I32) -> D,
            br(2, I32) -> R8,
            br(3, I32) -> R9,
            br(5, I32) -> A
          )), Vector(Def.Fun(Name.Global(List("foo")),
            List(br(0, I32), br(1, I32), br(2, I32), br(3, I32), br(4, I32)), I32, Vector(
              Block(Name.Local("body", 0), Vector(
                Inst.Move(register(0, I32), Op.Copy(br(0, I32))),
                Inst.Move(register(1, I32), Op.Copy(br(1, I32))),
                Inst.Move(register(2, I32), Op.Copy(br(2, I32))),
                Inst.Move(register(3, I32), Op.Copy(br(3, I32))),
                Inst.Move(register(4, I32), Op.Copy(br(4, I32))),
                Inst.Move(br(5, I32), Op.Copy(i32(10)))
              ), FlowControl.Return(br(5, I32)))
            ), Set.empty)))
      }
      "precolours for calls returning small structs" - {
        val struct = Type.Struct(Vector(I32, I64, I16, I8, U1))
        val r0 = register(0, struct)
        pass.txBlock(Block(Name.Local("body", 0), Vector(
          Inst.Move(r0, Op.Call(ir.Val.G(Name.Global(List("bar")), Fun(Nil, struct)), Nil)),
          Inst.Move(register(1, I32), Op.Member(r0, 0)),
          Inst.Move(register(2, I64), Op.Member(r0, 1)),
          Inst.Move(register(3, I16), Op.Member(r0, 2)),
          Inst.Move(register(4, I8), Op.Member(r0, 3)),
          Inst.Move(register(5, U1), Op.Member(r0, 4))
        ), FlowControl.Return(u0))).run.runA(pass.St()).value ==>
          ((Colours(int = Set(
            br(0, I32) -> A,
            br(1, I64) -> D,
            br(2, I16) -> C,
            br(3, I8) -> R8,
            br(4, U1) -> R9
          )), Vector(Block(Name.Local("body", 0), Vector(
            Inst.Do(Op.Call(ir.Val.G(Name.Global(List("bar")), Fun(Nil, struct)), Nil)),
            Inst.Move(br(0, I32), Op.Garbled),
            Inst.Move(br(1, I64), Op.Garbled),
            Inst.Move(br(2, I16), Op.Garbled),
            Inst.Move(br(3, I8),  Op.Garbled),
            Inst.Move(br(4, U1),  Op.Garbled),
            Inst.Move(register(1, I32), Op.Copy(br(0, I32))),
            Inst.Move(register(2, I64), Op.Copy(br(1, I64))),
            Inst.Move(register(3, I16), Op.Copy(br(2, I16))),
            Inst.Move(register(4, I8),  Op.Copy(br(3, I8))),
            Inst.Move(register(5, U1),  Op.Copy(br(4, U1)))
          ), FlowControl.Return(u0)))))
      }
      "return of small structs" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, Struct(Vector(I8, I16, I32, I64, U64)), Vector(
          Block(Name.Local("body", 0), Vector.empty,
            FlowControl.Return(ir.Val.Struct(Vector(i8(0), i16(0), i32(0), i64(0), u64(0)))))
        ), Set.empty)).run.runA(pass.St()).value ==>
          (Colours(int = Set(
            br(0, I8) -> A,
            br(1, I16) -> D,
            br(2, I32) -> C,
            br(3, I64) -> R8,
            br(4, U64) -> R9
          )), Vector(Def.Fun($.foo, Nil, Struct(Vector(I8, I16, I32, I64, U64)), Vector(
            Block(%body 0, Vector( 
              Inst.Move(br(0, I8), Op.Copy(i8(0))),
              Inst.Move(br(1, I16), Op.Copy(i16(0))),
              Inst.Move(br(2, I32), Op.Copy(i32(0))),
              Inst.Move(br(3, I64), Op.Copy(i64(0))),
              Inst.Move(br(4, U64), Op.Copy(u64(0)))
            ), FlowControl.Return(ir.Val.Struct(Vector(br(0, I8), br(1, I16), br(2, I32), br(3, I64), br(4, U64)))))
          ), Set.empty)))
      }
      "float" - {
        "def params and ret" - {
          pass.txDef(Def.Fun(Name.Global(List("foo")),
            List(register(0, F64), register(1, F32), register(2, F64),
              register(3, F64), register(4, F64), register(5, F64)),
            F32, Vector(
              Block(Name.Local("body", 0), Vector.empty,
                FlowControl.Return(register(1, F32)))
            ), Set.empty)).run.runA(pass.St()).value ==> (Colours(float = Set(
              br(0, F64) -> Xmm0,
              br(1, F32) -> Xmm1,
              br(2, F64) -> Xmm2,
              br(3, F64) -> Xmm3,
              br(6, F32) -> Xmm0
            )), Vector(Def.Fun(Name.Global(List("foo")),
              List(br(0, F64), br(1, F32), br(2, F64), br(3, F64), br(4, F64), br(5, F64)), F32, Vector(
                Block(Name.Local("body", 0), Vector(
                  Inst.Move(register(0, F64), Op.Copy(br(0, F64))),
                  Inst.Move(register(1, F32), Op.Copy(br(1, F32))),
                  Inst.Move(register(2, F64), Op.Copy(br(2, F64))),
                  Inst.Move(register(3, F64), Op.Copy(br(3, F64))),
                  Inst.Move(register(4, F64), Op.Copy(br(4, F64))),
                  Inst.Move(register(5, F64), Op.Copy(br(5, F64))),
                  Inst.Move(br(6, F32), Op.Copy(register(1, F32)))
                ), FlowControl.Return(br(6, F32)))
              ), Set.empty)))
        }
        "call params and ret" - {
          pass.txInstruction(Inst.Move(register(4, F64), Op.Call(ir.Val.G(Name.Global(List("foo")), Type.Fun(List.fill(6)(F32), F64)),
            List(register(0, F32), register(1, F32), register(2, F32), register(3, F32), register(5, F32), register(6, F32))))).run.runA(pass.St()).value ==>
            (Colours(float = Set(
              br(0, F32) -> Xmm0,
              br(1, F32) -> Xmm1,
              br(2, F32) -> Xmm2,
              br(3, F32) -> Xmm3,
              br(4, F64) -> Xmm0)),
              Vector(
                Inst.Move(br(0, F32), Op.Copy(register(0, F32))),
                Inst.Move(br(1, F32), Op.Copy(register(1, F32))),
                Inst.Move(br(2, F32), Op.Copy(register(2, F32))),
                Inst.Move(br(3, F32), Op.Copy(register(3, F32))),
                Inst.Move(br(4, F64), Op.Call(ir.Val.G(Name.Global(List("foo")), Type.Fun(List.fill(6)(F32), F64)),
                  List(br(0, F32), br(1, F32), br(2, F32), br(3, F32), register(5, F32), register(6, F32)))),
                Inst.Move(register(4, F64), Op.Copy(br(4, F64)))
              ))

        }
        "mixed int and float params in def" - {
          pass.txDef(Def.Fun(Name.Global(List("foo")),
            List(register(0, F64), register(1, I32), register(2, F32), register(3, I8)), F32, Vector(
              Block(Name.Local("body", 0), Vector.empty,
                FlowControl.Return(register(2, F32)))
            ), Set.empty)).run.runA(pass.St()).value ==>
            (Colours(int = Set(
              br(1, I32) -> D,
              br(3, I8)  -> R9
            ), float = Set(
              br(0, F64) -> Xmm0,
              br(2, F32) -> Xmm2,
              br(4, F32) -> Xmm0
            )), Vector(Def.Fun(Name.Global(List("foo")),
              List(br(0, F64), br(1, I32), br(2, F32), br(3, I8)), F32, Vector(
                Block(Name.Local("body", 0), Vector(
                  Inst.Move(register(0, F64), Op.Copy(br(0, F64))),
                  Inst.Move(register(1, I32), Op.Copy(br(1, I32))),
                  Inst.Move(register(2, F32), Op.Copy(br(2, F32))),
                  Inst.Move(register(3, I8),  Op.Copy(br(3, I8))),
                  Inst.Move(br(4, F32), Op.Copy(register(2, F32)))
                ), FlowControl.Return(br(4, F32)))
              ), Set.empty)))
        }
        "mixed int and float call params" - {
          pass.txInstruction(Inst.Move(register(4, F64), Op.Call(ir.Val.G(Name.Global(List("foo")), Type.Fun(List(I32, F32, I32, F32), F64)),
            List(register(0, I32), register(1, F32), register(2, I32), register(3, F32))))).run.runA(pass.St()).value ==>
            (Colours(
              int = Set(
                br(0, I32) -> C,
                br(2, I32) -> R8
              ),
              float = Set(
                br(1, F32) -> Xmm1,
                br(3, F32) -> Xmm3,
                br(4, F64) -> Xmm0
              )
            ), Vector(
              Inst.Move(br(0, I32), Op.Copy(ir.Val.R(register(0, I32)))),
              Inst.Move(br(1, F32), Op.Copy(ir.Val.R(register(1, F32)))),
              Inst.Move(br(2, I32), Op.Copy(ir.Val.R(register(2, I32)))),
              Inst.Move(br(3, F32), Op.Copy(ir.Val.R(register(3, F32)))),
              Inst.Move(br(4, F64), Op.Call(ir.Val.G(Name.Global(List("foo")), Type.Fun(List(I32, F32, I32, F32), F64)),
                List(br(0, I32), br(1, F32), br(2, I32), br(3, F32)))),
              Inst.Move(register(4, F64), Op.Copy(ir.Val.R(br(4, F64))))
            ))
        }
        "call to function returning struct with floats" - {
          val vec = Struct(Vector(F64, F64, F64, F64))
          pass.txBlock(Block(Name.Local("", 0), Vector(
            Inst.Move(register(0, vec), Op.Call(ir.Val.G(Name.Global(List("foo")), Fun(Nil, vec)), Nil)),
            Inst.Move(register(1, F64), Op.Member(register(0, vec), 0)),
            Inst.Move(register(2, F64), Op.Member(register(0, vec), 1)),
            Inst.Move(register(3, F64), Op.Member(register(0, vec), 2)),
            Inst.Move(register(4, F64), Op.Member(register(0, vec), 3))
          ), FlowControl.Return(u0))).run.runA(pass.St()).value ==>
            (Colours(float = Set(
              br(0, F64) -> Xmm0,
              br(1, F64) -> Xmm1,
              br(2, F64) -> Xmm2,
              br(3, F64) -> Xmm3
            )), Vector(Block(Name.Local("", 0), Vector(
              Inst.Do(Op.Call(ir.Val.G(Name.Global(List("foo")), Fun(Nil, vec)), Nil)),
              Inst.Move(br(0, F64), Op.Garbled),
              Inst.Move(br(1, F64), Op.Garbled),
              Inst.Move(br(2, F64), Op.Garbled),
              Inst.Move(br(3, F64), Op.Garbled),
              Inst.Move(register(1, F64), Op.Copy(br(0, F64))),
              Inst.Move(register(2, F64), Op.Copy(br(1, F64))),
              Inst.Move(register(3, F64), Op.Copy(br(2, F64))),
              Inst.Move(register(4, F64), Op.Copy(br(3, F64)))
            ), FlowControl.Return(u0))))
        }
        "def of function returning struct with floats" - {

          pass.txFlow(FlowControl.Return(
            ir.Val.Struct(Vector(register(0, F64), register(1, F64), register(2, F64), register(3, F64)))))
              .run.runA(pass.St()).value ==>
                (Colours(float = Set(
                  br(0, F64) -> Xmm0,
                  br(1, F64) -> Xmm1,
                  br(2, F64) -> Xmm2,
                  br(3, F64) -> Xmm3
                )), (Vector(
                  Inst.Move(br(0, F64), Op.Copy(register(0, F64))),
                  Inst.Move(br(1, F64), Op.Copy(register(1, F64))),
                  Inst.Move(br(2, F64), Op.Copy(register(2, F64))),
                  Inst.Move(br(3, F64), Op.Copy(register(3, F64)))
                ), FlowControl.Return(ir.Val.Struct(Vector(
                  br(0, F64), br(1, F64), br(2, F64), br(3, F64))))))
        }
      }
    }
    "txInstruction" - {
      "Call" - {
        pass.txInstruction(Inst.Move(register(0, I32), Op.Call(ir.Val.G($.foo, Fun(List.fill(6)(I32), I32)),
          List(i32(0), i32(1), i32(2), i32(3), i32(4), i32(5))))).run.runA(pass.St()).value ==>
          ((Colours(int = Set(
            br(0, I32) -> C,
            br(1, I32) -> D,
            br(2, I32) -> R8,
            br(3, I32) -> R9,
            br(4, I32) -> A
          )), Vector(
            Inst.Move(br(0, I32), Op.Copy(i32(0))),
            Inst.Move(br(1, I32), Op.Copy(i32(1))),
            Inst.Move(br(2, I32), Op.Copy(i32(2))),
            Inst.Move(br(3, I32), Op.Copy(i32(3))),
            Inst.Move(br(4, I32), Op.Call(ir.Val.G($.foo, Fun(List.fill(6)(I32), I32)),
              List(br(0, I32), br(1, I32), br(2, I32), br(3, I32), i32(4), i32(5)))),
            Inst.Move(register(0, I32), Op.Copy(br(4, I32)))
          )))
      }
      "div" - {
        pass.txInstruction(Inst.Move(register(2, I32), Op.Binary(InfixOp.Div, register(0, I32), register(1, I32))))
          .run.runA(pass.St()).value ==> (Colours(int = Set(
            br(0, I32) -> A,
            br(1, I32) -> D,
            br(3, I32) -> A,
            br(4, I32) -> D
          )), Vector(
            Inst.Move(br(2, I32), Op.Copy(register(1, I32))),
            Inst.Move(br(0, I32), Op.Copy(register(0, I32))),
            Inst.Move(br(1, I32), Op.Copy(0)),
            Inst.Do(Op.Copy(br(1, I32))),
            Inst.Move(br(3, I32), Op.Binary(InfixOp.Div, br(0, I32), br(2, I32))),
            Inst.Move(br(4, I32), Op.Garbled),
            Inst.Move(register(2, I32), Op.Copy(br(3, I32)))
          ))
      }
    }
  }

}
