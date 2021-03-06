package oxidation
package backend
package shared

import cats._
import cats.data._
import cats.implicits._

import codegen.Name

object RegisterLifetime {

  def inputs(block: ir.Block): Set[ir.Register] = {
    final case class Res(written: Set[ir.Register], input: Set[ir.Register])

    (block.instructions :+ ir.Inst.Flow(block.flow)).foldLeft(Res(Set.empty, Set.empty)) {
      case (r, ir.Inst.Label(_)) => r
      case (r, ir.Inst.Flow(flow)) =>
        val read = flow.reads
        val input = read diff r.written
        r.copy(input = r.input ++ input)
      case (r, ir.Inst.Move(dest, op)) =>
        val read = op.reads
        val input = read diff r.written
        Res(written = r.written + dest, input = r.input ++ input)
      case (r, ir.Inst.Do(op)) =>
        val read = op.reads
        val input = read diff r.written
        r.copy(input = r.input ++ input)
    }.input
  }

  def outputs(graph: FlowGraph, name: Name, inputs: Map[Name, Set[ir.Register]]): Set[ir.Register] = {
    val block = graph.blocks(name)
    val written: Set[ir.Register] = block.instructions.foldMap {
      case ir.Inst.Move(r, _) => Set(r)
      case _ => Set.empty
    }
    val read = graph.successors(name).foldMap(inputs(_))
    written & read
  }

  def ghosts(graph: FlowGraph, name: Name, inputs: Map[Name, Set[ir.Register]], outputs: Map[Name, Set[ir.Register]]): Set[ir.Register] = {
    val outsBefore = graph.predecessors(name).flatMap(b => outputs(b))
    val insAfter = graph.successors(name).flatMap(inputs(_))
    (outsBefore ++ inputs(name)) & (insAfter ++ outputs(name))
  }

  type Bound = Option[Int]

  def lifetime(reg: ir.Register, instrs: Vector[(ir.Inst, Int)], inputs: Set[ir.Register], outputs: Set[ir.Register]): (Bound, Bound) = {
    val firstWrite = if(inputs(reg)) None else instrs.collectFirst {
      case (ir.Inst.Move(`reg`, _), i) => i
    }
    val lastRead = if(outputs(reg)) None else instrs.collect {
      case (ir.Inst.Move(_, op), i) if op.reads(reg) => i
      case (ir.Inst.Do(op), i) if op.reads(reg) => i
      case (ir.Inst.Flow(flow), i) if flow.reads(reg) => i
    }.lastOption.orElse(firstWrite.map(_ + 1))
    firstWrite -> lastRead
  }

}
