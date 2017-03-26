package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._

trait Pass {

  type =?>[A, B] = PartialFunction[A, B]

  type F[A]
  implicit val F: Monad[F]
  def extract[A](f: F[A]): A

  def onDef: ir.Def =?> F[Vector[ir.Def]] = PartialFunction.empty

  def onBlock: ir.Block =?> F[Vector[ir.Block]] = PartialFunction.empty

  def onInstruction: ir.Inst =?> F[Vector[ir.Inst]] = PartialFunction.empty

  def onFlow: ir.FlowControl =?> F[ir.FlowControl] = PartialFunction.empty

  def txBlock(block: ir.Block): F[Vector[ir.Block]] = {
    val blocks = onBlock.lift(block).getOrElse(F.pure(Vector(block)))
    blocks.flatMap(_.traverse {
      case ir.Block(name, instrs, flow) =>
        val newInstrs = instrs.traverse(i => onInstruction.lift(i).getOrElse(F.pure(Vector(i)))).map(_.flatten)
        val newFlow = onFlow.lift(flow).getOrElse(F.pure(flow))
        (newInstrs, newFlow).map2(ir.Block(name, _, _))
    })
  }

  def txDef(d: ir.Def): F[Vector[ir.Def]] = {
    val defs = onDef.lift(d).getOrElse(F.pure(Vector(d)))
    defs.flatMap(_.traverse {
      case ir.Def.Fun(name, params, ret, body) =>
        val newBody = body.traverse(txBlock).map(_.flatten)
        newBody.map(ir.Def.Fun(name, params, ret, _))
    })
  }

}
