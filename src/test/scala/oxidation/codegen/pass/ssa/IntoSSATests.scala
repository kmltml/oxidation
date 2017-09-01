package oxidation
package codegen.pass
package ssa

import scala.language.dynamics

import codegen.{Codegen, Name}
import utest._
import ir._, Type._
import backend.shared.FlowGraph
import FlowControl._
import Op._
import InfixOp._

object IntoSSATests extends TestSuite with IrValSyntax {

  val pass = IntoSSA

  import Codegen.{ register => r }
  import pass.{ register => ssa }

  def txBlocks(blocks: Block*): Vector[Block] = {
    val ret = blocks.collectFirst {
      case Block(_, _, FlowControl.Return(v)) => v.typ
    }.getOrElse(U0)
    pass.extract(pass.txDef(Def.Fun(Name.Global(List("foo")), Nil, ret, blocks.toVector, Set.empty))) match {
      case Vector(Def.Fun(_, _, _, blocks, _)) => blocks
    }
  }

  object % extends Dynamic {
    def applyDynamic(name: String)(idx: Int): Name = Name.Local(name, idx)
  }
  case class GlobalBuilder(prefix: List[String]) extends Dynamic {
    def selectDynamic(name: String): GlobalBuilder = GlobalBuilder(name :: prefix)
    def toName: Name = Name.Global(prefix.reverse)
  }
  object GlobalBuilder {
    implicit def toName(b: GlobalBuilder): Name = b.toName
  }
  object $ extends GlobalBuilder(Nil)

  implicit class RegisterDSL(private val r: Register) extends AnyVal {

    def :=(op: Op): Inst = Inst.Move(r, op)
    def :=(v: Val): Inst = Inst.Move(r, Copy(v))

  }

  val tests = apply {
    "transformation" - {
      "Register renaming" - {
        txBlocks(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            Inst.Do(Call(Val.G($.printInt, Fun(List(I32), U0)), List(r(0, I32)))),
            r(0, I32) := i32(2),
            Inst.Do(Call(Val.G($.printInt, Fun(List(I32), U0)), List(r(0, I32))))
          ), Return(u0))
        ) ==> Vector(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            Inst.Do(Call(Val.G($.printInt, Fun(List(I32), U0)), List(r(0, I32)))),
            ssa(0, I32) := i32(2),
            Inst.Do(Call(Val.G($.printInt, Fun(List(I32), U0)), List(ssa(0, I32))))
          ), Return(u0))
        )
      }
      "Renaming usages in flow control" - {
        txBlocks(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            r(0, I32) := i32(1)
          ), Return(r(0, I32)))
        ) ==> Vector(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            ssa(0, I32) := i32(1)
          ), Return(ssa(0, I32)))
        )
      }
      "Across blocks" - {
        txBlocks(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            r(0, I32) := i32(1)
          ), Goto(% body 1)),
          Block(% body 1, Vector(
            r(1, I32) := Binary(Add, r(0, I32), i32(10))
          ), Return(r(0, I32)))
        ) ==> Vector(
          Block(% body 0, Vector(
            r(0, I32) := i32(0),
            ssa(0, I32) := i32(1)
          ), Goto(% body 1)),
          Block(% body 1, Vector(
            r(1, I32) := Binary(Add, ssa(0, I32), i32(10))
          ), Return(ssa(0, I32)))
        )
      }
      "An if expression" - {
        txBlocks(
          Block(% body 0, Vector(
            r(0, U1) := u1(true)
          ), Branch(r(0, U1), % `if` 0, % `else` 0)),
          Block(% `if` 0, Vector(
            r(1, I32) := i32(10)
          ), Goto(% ifafter 0)),
          Block(% `else` 0, Vector(
            r(1, I32) := i32(20)
          ), Goto(% ifafter 0)),
          Block(% ifafter 0, Vector.empty, Return(r(1, I32)))
        ) ==> Vector(
          Block(% body 0, Vector(
            r(0, U1) := u1(true)
          ), Branch(r(0, U1), % `if` 0, % `else` 0)),
          Block(% `if` 0, Vector(
            ssa(0, I32) := i32(10)
          ), Goto(% ifafter 0)),
          Block(% `else` 0, Vector(
            r(1, I32) := i32(20)
          ), Goto(% ifafter 0)),
          Block(% ifafter 0, Vector(
            ssa(1, I32) := Phi(Map((% `else` 0) -> r(1, I32), (% `if` 0) -> ssa(0, I32)))
          ), Return(ssa(1, I32)))
        )
      }
      "A while statement" - {
        txBlocks(
          Block(%body 0, Vector(
            r(0, I32) := 0
          ), Goto(%whilecond 0)),
          Block(%whilecond 0, Vector(
            r(1, U1) := Binary(Lt, r(0, I32), i32(10))
          ), Branch(r(1, U1), %whilebody 0, %whileafter 0)),
          Block(%whilebody 0, Vector(
            Inst.Do(Call(Val.G($.foo, Fun(List(I32), U0)), List(r(0, I32)))),
            r(0, I32) := Binary(Add, r(0, I32), i32(1))
          ), Goto(%whilecond 0)),
          Block(%whileafter 0, Vector.empty, Return(u0))
        ) ==> Vector(
          Block(%body 0, Vector(
            r(0, I32) := 0
          ), Goto(%whilecond 0)),
          Block(%whilecond 0, Vector(
            ssa(0, I32) := Phi(Map((%body 0) -> r(0, I32), (%whilebody 0) -> ssa(1, I32))),
            r(1, U1) := Binary(Lt, ssa(0, I32), i32(10))
          ), Branch(r(1, U1), %whilebody 0, %whileafter 0)),
          Block(%whilebody 0, Vector(
            Inst.Do(Call(Val.G($.foo, Fun(List(I32), U0)), List(ssa(0, I32)))),
            ssa(1, I32) := Binary(Add, ssa(0, I32), i32(1))
          ), Goto(%whilecond 0)),
          Block(%whileafter 0, Vector.empty, Return(u0))
        )
      }
    }
    "dominators" - {
      implicit def lbl(i: Int): Name = % a i
      val empty = Vector.empty[Inst]
      "Figure 2 in paper" - {
        val graph = FlowGraph(Seq(
          Block(5, empty, Branch(u0, 4, 3)),
          Block(4, empty, Goto(1)),
          Block(3, empty, Goto(2)),
          Block(2, empty, Goto(1)),
          Block(1, empty, Goto(2))
        ))
        pass.dominators(graph) ==> (1 to 5).map(i => lbl(i) -> lbl(5)).toMap
      }
      "Figure 4 in paper" - {
        val graph = FlowGraph(Seq(
          Block(6, empty, Branch(u0, 5, 4)),
          Block(5, empty, Goto(1)),
          Block(4, empty, Branch(u0, 2, 3)),
          Block(3, empty, Goto(2)),
          Block(2, empty, Branch(u0, 1, 3)),
          Block(1, empty, Goto(2))
        ))
        pass.dominators(graph) ==> (1 to 6).map(i => lbl(i) -> lbl(6)).toMap
      }
      "If in a while" - {
        val graph = FlowGraph(Seq(
          Block(6, empty, Branch(u0, 5, 4)),
          Block(5, empty, Return(u0)),
          Block(4, empty, Branch(u0, 3, 2)),
          Block(3, empty, Goto(1)),
          Block(2, empty, Goto(1)),
          Block(1, empty, Goto(6))
        ))
        pass.dominators(graph) ==> Map(
          lbl(6) -> lbl(6),
          lbl(5) -> lbl(6),
          lbl(4) -> lbl(6),
          lbl(3) -> lbl(4),
          lbl(2) -> lbl(4),
          lbl(1) -> lbl(4)
        )
      }
    }
    "frontiers" - {
      implicit def lbl(i: Int): Name = % a i
      val empty = Vector.empty[Inst]
      "If" - {
        val graph = FlowGraph(Seq(
          Block(1, empty, Branch(u0, 2, 3)),
          Block(2, empty, Goto(4)),
          Block(3, empty, Goto(4)),
          Block(4, empty, Return(u0))
        ))
        val doms = (1 to 4).map(i => lbl(i) -> lbl(1)).toMap
        pass.frontiers(graph, doms) ==> Map(
          lbl(2) -> Set(lbl(4)),
          lbl(3) -> Set(lbl(4))
        )
      }
    }
  }

}
