package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._

import codegen.{Name, ir}

object RegisterLifetime {

  def inputs(block: ir.Block): Set[ir.Register] = {
    final case class Res(written: Set[ir.Register], input: Set[ir.Register])

    def reads(op: ir.Op): Set[ir.Register] = {
      val vals = op match {
        case ir.Op.Arith(_, l, r) => Set(l, r)
        case ir.Op.Call(fn, params) => params.toSet + fn
        case ir.Op.Copy(v) => Set(v)
        case ir.Op.Unary(_, v) => Set(v)
      }
      vals.collect {
        case ir.Val.R(r) => r
      }
    }

    block.instructions.foldLeft(Res(Set.empty, Set.empty)) {
      case (r, ir.Inst.Label(_)) => r
      case (r, ir.Inst.Flow(_)) => r
      case (r, ir.Inst.Eval(dest, op)) =>
        val read = reads(op)
        val written = r.written ++ dest
        val input = read diff written
        Res(written = written, input = r.input ++ input)
    }.input
  }

  def outputs(graph: FlowGraph, name: Name, inputs: Map[Name, Set[ir.Register]]): Set[ir.Register] = {
    val block = graph.blocks(name)
    val written: Set[ir.Register] = block.instructions.foldMap {
      case ir.Inst.Eval(Some(r), _) => Set(r)
      case _ => Set.empty
    }
    val read = graph.successors(name).foldMap(b => inputs(b.name))
    written & read
  }

}
