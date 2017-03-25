package oxidation
package backend
package shared

import utest._
import codegen.Name
import codegen.ir._
import oxidation.codegen.ir.Type._

object RegisterLifetimeTests extends TestSuite {

  import RegisterLifetime._

  private implicit def vali(i: Int): Val = Val.I(i)
  private implicit def valr(r: Register): Val = Val.R(r)

  val tests = apply {
    "inputs" - {
      "just read" - {
        inputs(Block(Name.Local("test", 0), Vector(
          Inst.Do(Op.Arith(InfixOp.Add, Register(0, I32), Register(1, I32))),
          Inst.Do(Op.Copy(Register(2, I32))),
          Inst.Do(Op.Unary(PrefixOp.Neg, Register(3, I32))),
          Inst.Do(Op.Call(Register(4, I8), List(Register(5, I32))))
        ), FlowControl.Goto(Name.Local("test", 1)))) ==>
          Set(Register(0, I32), Register(1, I32), Register(2, I32), Register(3, I32), Register(4, I8), Register(5, I32))
      }
      "write-read" - {
        inputs(Block(Name.Local("test", 2), Vector(
          Inst.Move(Register(0, I32), Op.Copy(10)),
          Inst.Move(Register(1, I32), Op.Copy(Register(0, I32)))
        ), FlowControl.Goto(Name.Local("test", 3)))) ==> Set.empty
      }
      "read-write-read" - {
        inputs(Block(Name.Local("test", 4), Vector(
          Inst.Move(Register(0, I32), Op.Copy(Register(1, I32))),
          Inst.Move(Register(1, I32), Op.Copy(10)),
          Inst.Move(Register(2, I32), Op.Copy(Register(1, I32)))
        ), FlowControl.Goto(Name.Local("test", 5)))) ==> Set(Register(1, I32))
      }
      "read and write in one instruction" - {
        inputs(Block(Name.Local("test", 6), Vector(
          Inst.Move(Register(0, I32), Op.Arith(InfixOp.Add, Register(0, I32), 1))
        ), FlowControl.Goto(Name.Local("test", 7)))) ==> Set(Register(0, I32))
      }
      "read in flow" - {
        inputs(Block(Name.Local("test", 8), Vector.empty, FlowControl.Return(Register(0, I32)))) ==>
          Set(Register(0, I32))
      }
    }
    "outputs" - {
      "simple graph" - {
        val blocks = Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(Register(1, U1), Op.Arith(InfixOp.Lt, Register(0, I32), 0))
          ), FlowControl.Branch(Register(1, U1), Name.Local("if", 0), Name.Local("else", 0))),
          Block(Name.Local("if", 0), Vector(
            Inst.Move(Register(3, I32), Op.Unary(PrefixOp.Neg, Register(0, I32))),
            Inst.Move(Register(2, I32), Op.Copy(Register(3, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("else", 0), Vector(
            Inst.Move(Register(2, I32), Op.Copy(Register(0, I32)))
          ), FlowControl.Goto(Name.Local("ifafter", 0))),
          Block(Name.Local("ifafter", 0), Vector.empty, FlowControl.Return(Register(2, I32)))
        )
        val inputsByName = blocks.map(b => b.name -> inputs(b)).toMap
        val graph = FlowGraph(blocks)
        "body.0" - {
          outputs(graph, Name.Local("body", 0), inputsByName) ==>
            Set()
        }
        "if.0" - {
          outputs(graph, Name.Local("if", 0), inputsByName) ==>
            Set(Register(2, I32))
        }
        "else.0" - {
          outputs(graph, Name.Local("else", 0), inputsByName) ==>
            Set(Register(2, I32))
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
          0 -> Inst.Move(Register(0, I32), Op.Copy(2)),
          1 -> Inst.Move(Register(1, I32), Op.Copy(Register(3, I32))),
          2 -> Inst.Move(Register(2, I32), Op.Arith(InfixOp.Add, Register(0, I32), Register(2, I32))),
          3 -> Inst.Flow(FlowControl.Branch(Register(0, I32), null, null))
        ).map(_.swap)
        val ins = Set(Register(2, I32), Register(4, I32))
        val outs = Set(Register(2, I32), Register(4, I32))
        "r0 write-read" - {
          lifetime(Register(0, I32), insts, ins, outs) ==>
            Some(0) -> Some(3)
        }
        "r1 write-out" - {
          lifetime(Register(1, I32), insts, ins, outs) ==>
            Some(1) -> None
        }
        "r2 in-out" - {
          lifetime(Register(2, I32), insts, ins, outs) ==>
            None -> None
        }
        "r3 in-read" - {
          lifetime(Register(3, I32), insts, ins, outs) ==>
            None -> Some(1)
        }
        "r4 ghost" - {
          lifetime(Register(4, I32), insts, ins, outs) ==>
            None -> None
        }
      }
    }
  }

}
