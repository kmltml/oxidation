package oxidation
package codegen.pass
package ssa

import cats._
import cats.data._
import cats.implicits._

import ir._

object ConstantPropagation extends Pass {

  override val name = "constant-propagation"

  override type F[A] = State[Ctxt, A]
  override val F = implicitly[MonadState[F, Ctxt]]
  override def extract[A](fa: F[A]): A = fa.runA(new Ctxt(Map.empty)).value

  class Ctxt(val registers: Map[Register, Val]) extends AnyVal

  override val onDef = {
    case fun @ Def.Fun(name, params, _, blocks, _) =>
      val definitions = blocks.flatMap(_.instructions.collect {
        case Inst.Move(dest, Op.Phi(_)) => Vector(dest -> Op.Garbled) // Phi instructions can introduce cycles in the dependency graph
        case Inst.Move(dest, op) => Vector(dest -> op)
      }.combineAll).toMap
      val dependencies = definitions.mapValues(_.reads) ++ params.map(_ -> Set.empty[Register])
      def showReg(r: Register): String = s"${r.ns.prefix}${r.index}"
      def postorder(nodes: List[Register], visited: Set[Register]): Vector[Register] = nodes match {
        case Nil => Vector.empty
        case n :: rest if visited(n) => postorder(rest, visited)
        case n :: rest =>
          val deps = dependencies.getOrElse(n, Set.empty).toList
          val newVisited = visited + n
          val found = postorder(deps, newVisited)
          (found :+ n) ++ postorder(rest, newVisited ++ found)
      }
      val keys = definitions.keys.toList
      val order = postorder(keys, Set.empty) diff params
      assert(order.toSet == definitions.keySet)
      type M[A] = State[Map[Register, Option[Val]], A]
      val M = implicitly[MonadState[M, Map[Register, Option[Val]]]]

      val ctxt = new Ctxt(order.traverse_ { r =>
        for {
          solved <- M.get
          deps = dependencies(r).toList.traverse(d => solved(d).map(d -> _))
          solution = deps.flatMap { depList =>
            evalOp(definitions(r), r.typ)(new Ctxt(depList.toMap))
          }.flatMap {
            case Val(_, _: Type.Arr) => None
            case v => Some(v)
          }
          _ <- M.modify(_ + (r -> solution))
        } yield ()
      }.runS(params.map(_ -> None).toMap).value.flatMap { case (k, v) => v.map(k -> _) })
      F.set(ctxt) as Vector(fun)
    case d => F.set(new Ctxt(Map.empty)) as Vector(d)
  }

  override val onVal = {
    case v @ Val.R(r) => F.inspect { ctxt =>
      (Vector.empty, ctxt.registers.get(r).getOrElse(v))
    }
  }

  private def evalVal(v: Val)(implicit ctxt: Ctxt): Val = v match {
    case Val.R(r) => ctxt.registers(r)
    case Val.Struct(members) => Val.Struct(members.map(evalVal))
    case Val.Array(elems) => Val.Array(elems.map(evalVal))
    case e @ Val.Enum(_, members, _) => e.copy(members = members.map(evalVal))
    case v => v
  }

  private def arith(l: Val, r: Val)(integer: (Long, Long) => Long)(float: (Float, Float) => Float)
    (double: (Double, Double) => Double)(implicit ctxt: Ctxt): Option[Val] = l.typ match {
    case _ if l.typ != r.typ => None
    case Type.F32 =>
      val Val.F32(a) = evalVal(l)
      val Val.F32(b) = evalVal(r)
      Some(Val.F32(float(a, b)))
    case Type.F64 =>
      val Val.F64(a) = evalVal(l)
      val Val.F64(b) = evalVal(r)
      Some(Val.F64(double(a, b)))
    case _: Type.Integral =>
      val Val.I(a, _) = evalVal(l)
      val Val.I(b, _) = evalVal(r)
      Some(Val.I(integer(a, b), l.typ))
  }

  private def arithInt(l: Val, r: Val)(f: (Long, Long) => Long)(implicit ctxt: Ctxt): Option[Val] = l.typ match {
    case _ if l.typ != r.typ => None
    case _: Type.Integral | Type.U1 =>
      val Val.I(a, _) = evalVal(l)
      val Val.I(b, _) = evalVal(r)
      Some(Val.I(f(a, b), l.typ))
  }

  private def compare(l: Val, r: Val)(integer: (Long, Long) => Boolean)(float: (Float, Float) => Boolean)
    (double: (Double, Double) => Boolean)(implicit ctxt: Ctxt): Option[Val] = l.typ match {
    case _ if l.typ != r.typ => None
    case Type.F32 =>
      val Val.F32(a) = evalVal(l)
      val Val.F32(b) = evalVal(r)
      Some(Val.I(if(float(a, b)) 1 else 0, Type.U1))
    case Type.F64 =>
      val Val.F64(a) = evalVal(l)
      val Val.F64(b) = evalVal(r)
      Some(Val.I(if(double(a, b)) 1 else 0, Type.U1))
    case _: Type.Integral | Type.U1 =>
      val Val.I(a, _) = evalVal(l)
      val Val.I(b, _) = evalVal(r)
      Some(Val.I(if(integer(a, b)) 1 else 0, Type.U1))
  }

  private def ival(value: Long, typ: Type.Integral): Val = {
    val mask = typ.w match {
      case 64 => -1
      case w => (1l << w) - 1
    }
    Val.I(value & mask, typ)
  }

  private def evalOp(op: Op, destType: Type)(implicit ctxt: Ctxt): Option[Val] = op match {
    case Op.Binary(o, l, r) => o match {
      case InfixOp.Add => arith(l, r)(_ + _)(_ + _)(_ + _)
      case InfixOp.Sub => arith(l, r)(_ - _)(_ - _)(_ - _)
      case InfixOp.Div => arith(l, r)(_ / _)(_ / _)(_ / _)
      case InfixOp.Mod => arithInt(l, r)(_ % _)
      case InfixOp.Mul => arith(l, r)(_ * _)(_ * _)(_ * _)
      case InfixOp.Shl => arithInt(l, r)(_ << _)
      case InfixOp.Shr => l.typ match {
        case _: Type.U => arithInt(l, r)(_ >>> _)
        case _: Type.I => arithInt(l, r)(_ >> _)
        case _ => None
      }
      case InfixOp.BitAnd => arithInt(l, r)(_ & _)
      case InfixOp.BitOr => arithInt(l, r)(_ | _)
      case InfixOp.Xor => arithInt(l, r)(_ ^ _)
      case InfixOp.And | InfixOp.Or => None
      case InfixOp.Eq => compare(l, r)(_ == _)(_ == _)(_ == _)
      case InfixOp.Lt => compare(l, r)(_ < _)(_ < _)(_ < _)
      case InfixOp.Gt => compare(l, r)(_ > _)(_ > _)(_ > _)
      case InfixOp.Geq => compare(l, r)(_ >= _)(_ >= _)(_ >= _)
      case InfixOp.Leq => compare(l, r)(_ <= _)(_ <= _)(_ <= _)
      case InfixOp.Neq => compare(l, r)(_ != _)(_ != _)(_ != _)
    }
    case Op.Copy(s) => Some(evalVal(s))
    case Op.Unary(o, v) => (o, v.typ) match {
      case (PrefixOp.Neg, Type.F32) =>
        val Val.F32(a) = evalVal(v)
        Some(Val.F32(-a))
      case (PrefixOp.Neg, Type.F64) =>
        val Val.F64(a) = evalVal(v)
        Some(Val.F64(-a))
      case (PrefixOp.Neg, t: Type.I) =>
        val Val.I(a, _) = evalVal(v)
        Some(Val.I(-a, t))
      case (PrefixOp.Not, Type.U1) =>
        val Val.I(a, _) = evalVal(v)
        Some(Val.I(if(a == 0) 1 else 0, Type.U1))
      case (PrefixOp.Inv, t: Type.I) =>
        val Val.I(a, _) = evalVal(v)
        Some(Val.I(-a, t))
    }
    case Op.Widen(src) => evalVal(src) match {
      case v if v.typ == destType => Some(v)
      case Val.I(v, _) => Some(Val.I(v, destType))
      case Val.F32(v) if destType == Type.F64 => Some(Val.F64(v.toDouble))
      case _ => None
    }
    case Op.Trim(src) => evalVal(src) match {
      case v if v.typ == destType => Some(v)
      case Val.I(v, _) =>
        Some(ival(v, destType.asInstanceOf[Type.Integral]))
      case Val.F64(v) if destType == Type.F32 => Some(Val.F32(v.toFloat))
      case _ => None
    }
    case Op.Convert(v, to) => (evalVal(v), to) match {
      case (Val.F32(f), t: Type.Integral) => Some(ival(f.toLong, t))
      case (Val.F64(d), t: Type.Integral) =>
        Some(ival(d.toLong, t))
      case (Val.I(v, _), Type.F32) => Some(Val.F32(v.toFloat))
      case (Val.I(v, _), Type.F64) => Some(Val.F64(v.toDouble))
      case _ => None
    }
    case Op.Reinterpret(v, as) => (evalVal(v), as) match {
      case (Val.F32(f), t: Type.Integral) => Some(ival(java.lang.Float.floatToRawIntBits(f), t))
      case (Val.F64(f), t: Type.Integral) => Some(ival(java.lang.Double.doubleToRawLongBits(f), t))
      case (Val.I(l, _), Type.F32) => Some(Val.F32(java.lang.Float.intBitsToFloat(l.toInt)))
      case (Val.I(l, _), Type.F64) => Some(Val.F64(java.lang.Double.longBitsToDouble(l)))
      case _ => None
    }
    case Op.Sqrt(src) => evalVal(src) match {
      case Val.F32(f) => Some(Val.F32(math.sqrt(f).toFloat))
      case Val.F64(d) => Some(Val.F64(math.sqrt(d)))
      case _ => None
    }
    case Op.Phi(srcs) =>
      val srcVals = srcs.values.toSeq
      if(srcVals.distinct.size == 1) Some(Val.R(srcVals.head)) else None

    case Op.Member(_, _) | Op.StructCopy(_, _) | Op.TagOf(_) | Op.Unpack(_, _) =>
      throw new AssertionError(show"$op should not reach the constant propagation pass")
    case Op.Call(_, _) | Op.Load(_, _) | Op.Store(_, _, _) | Op.Stackalloc(_) | Op.Elem(_, _) |
         Op.ArrStore(_, _, _) | Op.Garbled => None
  }

}
