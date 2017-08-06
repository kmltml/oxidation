package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._
import oxidation.ir.Block

trait Pass {

  def name: String

  type =?>[A, B] = PartialFunction[A, B]

  type F[A]
  val F: Monad[F]
  private implicit def implicitF: Monad[F] = F
  def extract[A](f: F[A]): A

  def onDef: ir.Def =?> F[Vector[ir.Def]] = PartialFunction.empty

  def onBlock: ir.Block =?> F[Vector[ir.Block]] = PartialFunction.empty

  def onInstruction: ir.Inst =?> F[Vector[ir.Inst]] = PartialFunction.empty

  def onFlow: ir.FlowControl =?> F[(Vector[ir.Inst], ir.FlowControl)] = PartialFunction.empty

  def onVal: ir.Val =?> F[(Vector[ir.Inst], ir.Val)] = PartialFunction.empty

  def txVal(v: ir.Val): F[(Vector[ir.Inst], ir.Val)] = {
    onVal.lift(v).getOrElse(F.pure((Vector.empty, v)))
  }

  private def txValW(v: ir.Val): WriterT[F, Vector[ir.Inst], ir.Val] =
    WriterT(txVal(v))

  def txInstruction(inst: ir.Inst): F[Vector[ir.Inst]] = {
    onInstruction.lift(inst).getOrElse(F.pure(Vector(inst))).flatMap(_.traverse {
      case ir.Inst.Eval(dest, op) => (op match {
        case ir.Op.Binary(op, left, right) => (txValW(left), txValW(right)).map2(ir.Op.Binary(op, _, _))
        case ir.Op.Copy(src) => txValW(src).map(ir.Op.Copy)
        case ir.Op.Call(fn, params) => txValW(fn).map(ir.Op.Call(_, params)) // TODO params too?
        case ir.Op.Unary(op, right) => txValW(right).map(ir.Op.Unary(op, _))
        case ir.Op.Load(addr, offset) => (txValW(addr), txValW(offset)).map2(ir.Op.Load)
        case ir.Op.Store(addr, offset, value) => (txValW(addr), txValW(offset), txValW(value)).map3(ir.Op.Store)
        case ir.Op.Widen(v) => txValW(v).map(ir.Op.Widen)
        case ir.Op.Trim(v) => txValW(v).map(ir.Op.Trim)
        case ir.Op.Convert(v, t) => txValW(v).map(ir.Op.Convert(_, t))
        case ir.Op.Reinterpret(v, t) => txValW(v).map(ir.Op.Reinterpret(_, t))
        case ir.Op.Member(src, index) => txValW(src).map(ir.Op.Member(_, index))
        case o: ir.Op.Stackalloc => WriterT(F.pure((Vector.empty, o)))
        case ir.Op.StructCopy(src, substs) =>
          val s = substs.toList.traverse { case (i, v) => txValW(v).map(i -> _) }.map(_.toMap)
          (txValW(src), s).map2(ir.Op.StructCopy)
        case ir.Op.Elem(arr, index) => (txValW(arr), txValW(index)).map2(ir.Op.Elem)
        case ir.Op.ArrStore(arr, index, value) => (txValW(arr), txValW(index), txValW(value)).map3(ir.Op.ArrStore)
        case ir.Op.Garbled => WriterT(F.pure((Vector.empty, ir.Op.Garbled)))
        case ir.Op.Sqrt(s) => txValW(s).map(ir.Op.Sqrt)
        case ir.Op.TagOf(v) => txValW(v).map(ir.Op.TagOf)
        case ir.Op.Unpack(s, v) => txValW(s).map(ir.Op.Unpack(_, v))
      }).run.map { case (insts, o) => insts :+ ir.Inst.Eval(dest, o) }

      case lbl: ir.Inst.Label => F.pure(Vector[ir.Inst](lbl))
      case ir.Inst.Flow(flow) =>
        for {
          f <- txFlow(flow)
          pres <- f._1.traverse(txInstruction)
        } yield pres.flatten :+ ir.Inst.Flow(f._2)
    }).map(_.flatten)
  }

  def txFlow(flow: ir.FlowControl): F[(Vector[ir.Inst], ir.FlowControl)] = {
    onFlow.lift(flow).getOrElse(F.pure((Vector.empty, flow))).flatMap {
      case (pre, ir.FlowControl.Return(v)) =>
        txValW(v).map(ir.FlowControl.Return).run
          .map { case (is, f) => (pre ++ is, f) }
      case (pre, goto: ir.FlowControl.Goto) =>
        F.pure((pre, goto))
      case (pre, ir.FlowControl.Branch(cond, ifTrue, ifFalse)) =>
        txValW(cond).map(ir.FlowControl.Branch(_, ifTrue, ifFalse)).run
          .map { case (is, f) => (pre ++ is, f) }
    }
  }

  def txBlock(block: ir.Block): F[Vector[ir.Block]] = {
    val blocks = onBlock.lift(block).getOrElse(F.pure(Vector(block)))
    blocks.flatMap(_.traverse {
      case ir.Block(name, instrs, flow) =>
        val newInstrs = instrs.traverse(txInstruction).map(_.flatten)
        val newFlow = txFlow(flow)
        (newInstrs, newFlow).map2 { case (instrs, (pres, flo)) => ir.Block(name, instrs ++ pres, flo) }
    })
  }

  def txDef(d: ir.Def): F[Vector[ir.Def]] = {
    val defs = onDef.lift(d).getOrElse(F.pure(Vector(d)))
    defs.flatMap(_.traverse {
      case ir.Def.Fun(name, params, ret, body, cp) =>
        val newBody = body.traverse(txBlock).map(_.flatten)
        newBody.map((body: Vector[Block]) => ir.Def.Fun(name, params, ret, body, cp))
      case ir.Def.ComputedVal(name, body, typ, constantPool) =>
        val newBody = body.traverse(txBlock).map(_.flatten)
        newBody.map(body => ir.Def.ComputedVal(name, body, typ, constantPool))
      case efun: ir.Def.ExternFun => F.pure(efun)
      case ir.Def.TrivialVal(n, v) => txVal(v).map {
        case (Vector(), v) => ir.Def.TrivialVal(n, v)

        //TODO maybe some constant search should be done below?
        case (is, v) => ir.Def.ComputedVal(n, Vector(
          ir.Block(Name.Local("body", 0), is, ir.FlowControl.Return(v))), v.typ, Set.empty)
      }
    })
  }

}
