package oxidation
package backend
package shared

import utest._
import codegen.{Codegen, Name}
import ir._
import ir.Type._

object RegisterLifetimeTests extends TestSuite with IrValSyntax {

  import RegisterLifetime._

  import Codegen.register

  val tests = apply {
    "inputs" - {
      "just read" - {
        inputs(Block(Name.Local("test", 0), Vector(
          Inst.Do(Op.Arith(InfixOp.Add, register(0, I32), register(1, I32))),
          Inst.Do(Op.Copy(register(2, I32))),
          Inst.Do(Op.Unary(PrefixOp.Neg, register(3, I32))),
          Inst.Do(Op.Call(register(4, I8), List(register(5, I32))))
        ), FlowControl.Goto(Name.Local("test", 1)))) ==>
          Set(register(0, I32), register(1, I32), register(2, I32), register(3, I32), register(4, I8), register(5, I32))
      }
      "write-read" - {
        inputs(Block(Name.Local("test", 2), Vector(
          Inst.Move(register(0, I32), Op.Copy(10)),
          Inst.Move(register(1, I32), Op.Copy(register(0, I32)))
        ), FlowControl.Goto(Name.Local("test", 3)))) ==> Set.empty
      }
      "read-write-read" - {
        inputs(Block(Name.Local("test", 4), Vector(
          Inst.Move(register(0, I32), Op.Copy(register(1, I32))),
          Inst.Move(register(1, I32), Op.Copy(10)),
          Inst.Move(register(2, I32), Op.Copy(register(1, I32)))
        ), FlowControl.Goto(Name.Local("test", 5)))) ==> Set(register(1, I32))
      }
      "read and write in one instruction" - {
        inputs(Block(Name.Local("test", 6), Vector(
          Inst.Move(register(0, I32), Op.Arith(InfixOp.Add, register(0, I32), 1))
        ), FlowControl.Goto(Name.Local("test", 7)))) ==> Set(register(0, I32))
      }
      "read in flow" - {
        inputs(Block(Name.Local("test", 8), Vector.empty, FlowControl.Return(register(0, I32)))) ==>
          Set(register(0, I32))
      }
    }
    "outputs" - {
      "simple graph" - {
        val blocks = Vector(
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
        )
        val inputsByName = blocks.map(b => b.name -> inputs(b)).toMap
        val graph = FlowGraph(blocks)
        "body.0" - {
          outputs(graph, Name.Local("body", 0), inputsByName) ==>
            Set()
        }
        "if.0" - {
          outputs(graph, Name.Local("if", 0), inputsByName) ==>
            Set(register(2, I32))
        }
        "else.0" - {
          outputs(graph, Name.Local("else", 0), inputsByName) ==>
            Set(register(2, I32))
        }
        "ifafter.0" - {
          outputs(graph, Name.Local("ifafter", 0), inputsByName) ==>
            Set()
        }
      }
    }
    "lifetimes" - {
      "simple block" - {
        val insts = Vector(
          0 -> Inst.Move(register(0, I32), Op.Copy(2)),
          1 -> Inst.Move(register(1, I32), Op.Copy(register(3, I32))),
          2 -> Inst.Move(register(5, I32), Op.Copy(5)),
          3 -> Inst.Move(register(2, I32), Op.Arith(InfixOp.Add, register(0, I32), register(2, I32))),
          4 -> Inst.Flow(FlowControl.Branch(register(0, I32), null, null))
        ).map(_.swap)
        val ins = Set(register(2, I32), register(4, I32))
        val outs = Set(register(1, I32), register(2, I32), register(4, I32))
        "r0 write-read" - {
          lifetime(register(0, I32), insts, ins, outs) ==>
            Some(0) -> Some(4)
        }
        "r1 write-out" - {
          lifetime(register(1, I32), insts, ins, outs) ==>
            Some(1) -> None
        }
        "r2 in-out" - {
          lifetime(register(2, I32), insts, ins, outs) ==>
            None -> None
        }
        "r3 in-read" - {
          lifetime(register(3, I32), insts, ins, outs) ==>
            None -> Some(1)
        }
        "r4 ghost" - {
          lifetime(register(4, I32), insts, ins, outs) ==>
            None -> None
        }
        "r5 write-forget" - {
          lifetime(register(5, I32), insts, ins, outs) ==>
            Some(2) -> Some(3)
        }
      }
    }
  }

}
