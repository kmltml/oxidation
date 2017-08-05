package oxidation
package codegen.pass

import utest._

import ir._
import Type._
import InfixOp._
import codegen.{Name, Codegen}

object EnumLoweringPassTests extends TestSuite with IrValSyntax {

  val pass = EnumLoweringPass

  import Codegen.{register => r}
  import pass.{register => el}

  val option = Enum(List(
    Struct(Vector(I32)),
    Struct(Vector.empty)
  ))
  val optionRepr = Struct(Vector(U64))

  val tree = Enum(List(
    Struct(Vector(I32, Ptr, Ptr)),
    Struct(Vector.empty)
  ))
  val treeRepr = Struct(Vector.fill(3)(U64))

  val tests = apply {
    "TagOf" - {
      pass.txInstruction(
        Inst.Move(r(1, U8), Op.TagOf(r(0, option)))
      ).runA(pass.S()).value ==> Vector(
        Inst.Move(el(0, U64), Op.Member(r(0, optionRepr), 0)),
        Inst.Move(el(1, U64), Op.Binary(Shr, el(0, U64), u64(0))),
        Inst.Move(el(2, U8), Op.Trim(el(1, U64))),
        Inst.Move(r(1, U8), Op.Copy(el(2, U8)))
      )
    }
    "Unpack" - {
      val r0 = r(0, treeRepr)
      pass.txInstruction(
        Inst.Move(r(1, Struct(Vector(I32, Ptr, Ptr))), Op.Unpack(r(0, tree), 0))
      ).runA(pass.S()).value ==> Vector(
        Inst.Move(el(0, U64), Op.Member(r0, 0)),
        Inst.Move(el(1, U64), Op.Binary(Shr, el(0, U64), u64(8))),
        Inst.Move(el(2, I32), Op.Trim(el(1, U64))),

        Inst.Move(el(3, U64), Op.Member(r0, 1)),
        Inst.Move(el(4, U64), Op.Binary(Shr, el(3, U64), u64(0))),
        Inst.Move(el(5, Ptr), Op.Reinterpret(el(4, U64), Ptr)),

        Inst.Move(el(6, U64), Op.Member(r0, 2)),
        Inst.Move(el(7, U64), Op.Binary(Shr, el(6, U64), u64(0))),
        Inst.Move(el(8, Ptr), Op.Reinterpret(el(7, U64), Ptr)),

        Inst.Move(r(1, Struct(Vector(I32, Ptr, Ptr))), Op.Copy(
          Val.Struct(Vector(el(2, I32), el(5, Ptr), el(8, Ptr)))
        ))
      )
    }
    "Val.Enum" - {
      pass.txInstruction(
        Inst.Move(r(1, option), Op.Copy(Val.Enum(0, Vector(r(0, I32)), option)))
      ).runA(pass.S()).value ==> Vector(
        Inst.Move(el(0, U64), Op.Widen(u8(0))),
        Inst.Move(el(1, U64), Op.Binary(Shl, el(0, U64), u64(0))),
        Inst.Move(el(2, U64), Op.Widen(r(0, I32))),
        Inst.Move(el(3, U64), Op.Binary(Shl, el(2, U64), u64(8))),
        Inst.Move(el(4, U64), Op.Binary(BitOr, el(1, U64), el(3, U64))),
        Inst.Move(r(1, optionRepr), Op.Copy(Val.Struct(Vector(el(4, U64)))))
      )
    }
    "Call" - {
      pass.txInstruction(
        Inst.Move(r(1, I32), Op.Call(Val.G(Name.Global(List("foo")), Fun(List(option), I32)), List(r(0, option))))
      ).runA(pass.S()).value ==> Vector(
        Inst.Move(r(1, I32), Op.Call(Val.G(Name.Global(List("foo")), Fun(List(optionRepr), I32)), List(r(0, optionRepr))))
      )
    }
    "Def.Fun" - {
      "taking enum params" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), List(r(0, option)), U0, Vector.empty, Set.empty))
          .runA(pass.S()).value ==> Vector(
            Def.Fun(Name.Global(List("foo")), List(r(0, optionRepr)), U0, Vector.empty, Set.empty)
          )
      }
      "returning an enum" - {
        pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, option, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(r(0, option), Op.Copy(Val.Enum(1, Vector.empty, option)))
          ), FlowControl.Return(Val.R(r(0, option))))
        ), Set.empty)).runA(pass.S()).value ==> Vector(
          Def.Fun(Name.Global(List("foo")), Nil, optionRepr, Vector(
            Block(Name.Local("body", 0), Vector(
              Inst.Move(el(0, U64), Op.Widen(u8(1))),
              Inst.Move(el(1, U64), Op.Binary(Shl, el(0, U64), u64(0))),
              Inst.Move(r(0, optionRepr), Op.Copy(Val.Struct(Vector(el(1, U64)))))
            ), FlowControl.Return(Val.R(r(0, optionRepr))))
          ), Set.empty)
        )
      }
    }
  }
  
}
