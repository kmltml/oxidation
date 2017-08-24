package oxidation
package codegen.pass

import utest._

import ir._
import Type._
import InfixOp._
import codegen.{Name, Codegen}


object ConstantInliningTests extends TestSuite with IrValSyntax {

  val pass = ConstantInlining

  import Codegen.{ register => r }

  val tests = apply {
    "Simple TrivialVal" - {
      pass.txDefs(Vector(
        Def.TrivialVal(Name.Global(List("foo")), i32(42)),
        Def.Fun(Name.Global(List("bar")), Nil, I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(r(0, I32), Op.Load(Val.GlobalAddr(Name.Global(List("foo"))), i64(0)))
          ), FlowControl.Return(r(0, I32)))
        ), Set.empty)
      )).run(Map.empty) ==> Vector(
        Def.TrivialVal(Name.Global(List("foo")), i32(42)),
        Def.Fun(Name.Global(List("bar")), Nil, I32, Vector(
          Block(Name.Local("body", 0), Vector(
            Inst.Move(r(0, I32), Op.Copy(i32(42)))
          ), FlowControl.Return(r(0, I32)))
        ), Set.empty)
      )
    }
  }

}
