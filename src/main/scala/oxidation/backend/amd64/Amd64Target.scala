package oxidation
package backend
package amd64

import Reg._

import cats._
import cats.data._
import cats.implicits._

import codegen.ir

class Amd64Target { this: Output =>

  def outputInstruction(i: ir.Inst): M = i match {
    case ir.Inst.Label(n) => label(n)
    case ir.Inst.Eval(dest, op) => op match {
      case ir.Op.Arith(InfixOp.Add, left, right) => dest match {
        case None => M.empty
        case Some(r) => Vector(
          mov(reg(r), v(left)),
          add(reg(r), v(right))
        ).foldMap(identity)
      }
    }
  }

  def outputFlow(f: ir.FlowControl): M = f match {
    case ir.FlowControl.Return(r) => Vector(
      mov(RAX, v(r)),
      epilogue,
      ret
    ).foldMap(identity)
  }

  def epilogue: M = Vector(
    mov(RSP, RBP),
    pop(RBP)
  ).foldMap(identity)

  private def v(x: ir.Val): Val = x match {
    case ir.Val.I(i) => Val.I(i)
    case ir.Val.R(r) => reg(r)
  }

  private def reg(r: ir.Register): Val.R =
    Val.R(Seq(RCX, RDX, R8, R9, RAX, R10, R11)(r.index))

}
