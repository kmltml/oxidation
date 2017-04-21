package oxidation
package backend
package shared

import utest._
import codegen.{Codegen, Name}
import ir._
import ir.Type._

object RegisterAllocatorTests extends TestSuite {

  private implicit def vali(i: Int): Val = Val.I(i, ir.Type.I32)
  private implicit def valr(r: Register): Val = Val.R(r)

  import Codegen.register

  val allocator = new RegisterAllocator[Int](
    calleeSavedRegs = List(0, 1, 2, 3, 4),
    callerSavedRegs = List(5, 6, 7, 8, 9)
  ) {
    def rebuildAfterSpill(fun: Def.Fun, spilled: Set[Register]): Def.Fun = fun
  }

  def vr(index: Int, typ: Type): Register = Register(allocator.VirtualReg, index, typ)

  val tests = apply {
    "buildInterferenceGraph" - {
      "simple graph" - {
        val fun = Def.Fun(Name.Global(List("abs")), List(register(0, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(1, U1), Op.Binary(InfixOp.Lt, register(0, I32), 0))
          ), FlowControl.Branch(register(1, U1), Name.Local("if", 0), Name.Local("else", 0))),
          Block(Name.Local("if", 0), Vector(
            Inst.Move(register(3, I32), Op.Unary(PrefixOp.Neg, register(0, I32))),
            Inst.Move(register(2, I32), Op.Copy(register(3, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("else", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("ifafter", 0), Vector.empty, FlowControl.Return(register(2, I32)))
        ), Set.empty)
        allocator.buildInterferenceGraph(fun) ==> InterferenceGraph[Register, Int](
          colours = Map.empty,
          interferenceNeighbors = Map(
            register(0, I32) -> Set(register(1, U1)),
            register(1, U1) -> Set(register(0, I32))
          ),
          preferenceNeighbors = Map(
            register(2, I32) -> Set(register(3, I32), register(0, I32)),
            register(0, I32) -> Set(register(2, I32)),
            register(3, I32) -> Set(register(2, I32))
          )
        ).withNodes(Set(register(0, I32), register(1, U1), register(2, I32), register(3, I32)))
      }
      "variables around function call" - {
        val fun = Def.Fun(Name.Global(List("foo")), List(register(0, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(1, I32), Op.Copy(register(0, I32))),
            Inst.Move(register(2, I32), Op.Copy(10)),
            Inst.Move(register(3, I32), Op.Copy(20)),
            Inst.Move(register(4, I32), Op.Copy(register(1, I32))),
            Inst.Move(register(5, I32),
              Op.Call(Val.G(Name.Global(List("add")), Fun(List(I32, I32), I32)),
                List(register(3, I32), register(4, I32)))),
            Inst.Move(register(6, I32), Op.Copy(register(5, I32))),
            Inst.Move(register(7, I32), Op.Binary(InfixOp.Add, register(2, I32), register(6, I32))),
            Inst.Move(register(8, I32), Op.Copy(register(7, I32)))
          ), FlowControl.Return(register(8, I32)))
        ), Set.empty)
        allocator.buildInterferenceGraph(fun) ==> InterferenceGraph[Register, Int](
          colours = allocator.virtualRegs,
          interferenceNeighbors = Map.empty, preferenceNeighbors = Map.empty)
          .withInterferenceEdges(Set(
            register(1, I32) -> register(2, I32),
            register(1, I32) -> register(3, I32),
            register(2, I32) -> register(3, I32),
            register(2, I32) -> register(4, I32),
            register(2, I32) -> register(5, I32),
            register(2, I32) -> register(6, I32),
            register(2, I32) -> vr(0, U0),
            register(2, I32) -> vr(1, U0),
            register(2, I32) -> vr(2, U0),
            register(2, I32) -> vr(3, U0),
            register(2, I32) -> vr(4, U0),
            register(3, I32) -> register(4, I32)
          )).withPreferenceEdges(Set(
            register(0, I32) -> register(1, I32),
            register(4, I32) -> register(1, I32),
            register(5, I32) -> register(6, I32),
            register(8, I32) -> register(7, I32)
          )).withNodes(Set(register(0, I32), register(1, I32), register(2, I32), register(3, I32),
            register(4, I32), register(5, I32), register(6, I32), register(7, I32), register(8, I32),
            vr(0, U0), vr(1, U0), vr(2, U0), vr(3, U0), vr(4, U0)))
      }
    }
  }

}
