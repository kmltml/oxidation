package oxidation
package backend
package shared

import utest._
import codegen.{Codegen, Name}
import ir._
import ir.Type._
import <->.EdgeSyntax

object RegisterAllocatorTests extends TestSuite {

  private implicit def vali(i: Int): Val = Val.I(i, ir.Type.I32)
  private implicit def valr(r: Register): Val = Val.R(r)

  import Codegen.register

  val allocator = new RegisterAllocator[Int](
    calleeSavedRegs = List(0, 1, 2, 3, 4),
    callerSavedRegs = List(5, 6, 7, 8, 9)
  )

  val tests = apply {
    "buildInterferenceGraph" - {
      "simple graph" - {
        val fun = Def.Fun(Name.Global(List("abs")), List(register(0, I32)), I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(register(1, U1), Op.Arith(InfixOp.Lt, register(0, I32), 0))
          ), FlowControl.Branch(register(1, U1), Name.Local("if", 0), Name.Local("else", 0))),
          Block(Name.Local("if", 0), Vector(
            Inst.Move(register(3, I32), Op.Unary(PrefixOp.Neg, register(0, I32))),
            Inst.Move(register(2, I32), Op.Copy(register(3, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("else", 0), Vector(
            Inst.Move(register(2, I32), Op.Copy(register(0, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("ifafter", 0), Vector.empty, FlowControl.Return(register(2, I32)))
        ))
        allocator.buildInterferenceGraph(fun) ==> InterferenceGraph[Register, Int](
          nodes = Set(register(0, I32), register(1, U1), register(2, I32), register(3, I32)),
          colours = Map.empty,
          interferenceEdges = Set(
            register(0, I32) <-> register(1, U1)
          ),
          preferenceEdges = Set(
            register(2, I32) <-> register(3, I32),
            register(2, I32) <-> register(0, I32)
          )
        )
      }
    }
  }

}
