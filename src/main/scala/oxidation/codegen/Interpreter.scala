package oxidation
package codegen

import ir._

import cats._
import cats.data._
import cats.implicits._

object Interpreter {

  sealed trait V
  object V {
    final case class I(value: Long) extends V
    final case class F32(value: Float) extends V
    final case class F64(value: Double) extends V
    final case class S(members: Vector[V]) extends V
    final case class A(elems: List[V]) extends V
    final case object U0 extends V

    def fromVal(v: Val): F[V] = v match {
      case Val.I(i, _) => F.pure(I(i))
      case Val.F32(f) => F.pure(F32(f))
      case Val.F64(d) => F.pure(F64(d))
      case Val.R(r) => F.inspect(_.registers(r))
      case Val.Struct(members) => members.traverse(fromVal).map(S)
      case Val.Array(elems) => elems.traverse(fromVal).map(A)
    }

    def fromBool(v: Boolean): V = v match {
      case true => V.I(1)
      case false => V.I(0)
    }

  }

  final case class St(registers: Map[Register, V]) {

    def assign(reg: Register, value: V): St = copy(registers = registers + (reg -> value))

  }

  private type F[A] = State[St, A]
  private val F: MonadState[F, St] = implicitly

  def interpret(body: Vector[Block]): V = {
    val blocksByName = body.map(b => b.name -> b).toMap
    def interpretBlock(block: Block): F[V] = {
      for {
        _ <- block.instructions.traverse_(step)
        v <- block.flow match {
          case FlowControl.Return(v) => V.fromVal(v)
          case FlowControl.Goto(l) => interpretBlock(blocksByName(l))
          case FlowControl.Branch(cond, t, f) => V.fromVal(cond).flatMap {
            case V.I(0) => interpretBlock(blocksByName(f))
            case _ => interpretBlock(blocksByName(t))
          }
        }
      } yield v
    }
    interpretBlock(body.head).runA(St(Map.empty)).value
  }

  def step(inst: Inst): F[Unit] = inst match {
    case Inst.Eval(dest, op) =>
      for {
        v <- eval(op, dest.map(_.typ))
        _ <- dest.traverse_(d => F.modify(_.assign(d, v)))
      } yield ()

  }

  def eval(op: Op, destType: Option[ir.Type]): F[V] = op match {
    case Op.Copy(v) => V.fromVal(v)
    case Op.Widen(v) => V.fromVal(v)
    case Op.Trim(v) => V.fromVal(v).map {
      case V.I(i) => destType match {
        case Some(Type.I(w)) => V.I(i & ((1 << w) - 1))
        case Some(Type.U(w)) => V.I(i & ((1 << w) - 1))
        case _ => V.I(i)
      }
      case v: V.F32 => v
      case V.F64(d) => destType match {
        case Some(Type.F32) => V.F32(d.toFloat)
        case _ => V.F64(d)
      }
    }
    case Op.Member(src, index) =>
      V.fromVal(src).map {
        case V.S(members) => members(index)
      }
    case Op.StructCopy(src, substs) =>
      V.fromVal(src).flatMap {
        case V.S(members) => members.zipWithIndex.traverse {
          case (m, i) => substs.get(i).traverse(V.fromVal).map(_ getOrElse m)
        }.map(V.S)
      }
    case Op.Elem(arr, index) => (V.fromVal(arr), V.fromVal(index)).map2 {
      case (V.A(elems), V.I(i)) => elems(i.toInt)
    }
    case Op.ArrStore(Val.R(arr), index, value) => (F.inspect(_.registers(arr)), V.fromVal(index), V.fromVal(value)).map3(Tuple3.apply).flatMap {
      case (V.A(elems), V.I(i), v) => F.modify(_.assign(arr, V.A(elems.updated(i.toInt, v)))) as V.U0
    }
    case Op.Unary(op, v) => V.fromVal(v).map {
      case V.I(i) => op match {
        case PrefixOp.Neg => V.I(-i)
        case PrefixOp.Not => if(i == 0) V.I(1) else V.I(0)
        case PrefixOp.Inv => V.I(~i)
      }
      case V.F32(f) => op match {
        case PrefixOp.Neg => V.F32(-f)
      }
      case V.F64(d) => op match {
        case PrefixOp.Neg => V.F64(-d)
      }
    }
    case Op.Binary(op, av, bv) => (V.fromVal(av), V.fromVal(bv)).map2 {
      case (V.I(a), V.I(b)) => op match {
        case InfixOp.Add => V.I(a + b)
        case InfixOp.Sub => V.I(a - b)
        case InfixOp.Mul => V.I(a * b)
        case InfixOp.Div => V.I(a / b)
        case InfixOp.Mod => V.I(a % b)
        case InfixOp.Shr => av.typ match {
          case _: Type.U => V.I(a >>> b)
          case _ => V.I(a >> b)
        }
        case InfixOp.Shl => V.I(a << b)
        case InfixOp.BitAnd => V.I(a & b)
        case InfixOp.BitOr => V.I(a | b)
        case InfixOp.Xor => V.I(a ^ b)
        case InfixOp.Eq => V.fromBool(a == b)
        case InfixOp.Lt => V.fromBool(a < b)
        case InfixOp.Gt => V.fromBool(a > b)
        case InfixOp.Geq => V.fromBool(a >= b)
        case InfixOp.Leq => V.fromBool(a <= b)
        case InfixOp.Neq => V.fromBool(a != b)
      }
      case (V.F32(a), V.F32(b)) => op match {
        case InfixOp.Add => V.F32(a + b)
        case InfixOp.Sub => V.F32(a - b)
        case InfixOp.Mul => V.F32(a * b)
        case InfixOp.Div => V.F32(a / b)
        case InfixOp.Eq => V.fromBool(a == b)
        case InfixOp.Lt => V.fromBool(a < b)
        case InfixOp.Gt => V.fromBool(a > b)
        case InfixOp.Geq => V.fromBool(a >= b)
        case InfixOp.Leq => V.fromBool(a <= b)
        case InfixOp.Neq => V.fromBool(a != b)
      }
      case (V.F64(a), V.F64(b)) => op match {
        case InfixOp.Add => V.F64(a + b)
        case InfixOp.Sub => V.F64(a - b)
        case InfixOp.Mul => V.F64(a * b)
        case InfixOp.Div => V.F64(a / b)
        case InfixOp.Eq => V.fromBool(a == b)
        case InfixOp.Lt => V.fromBool(a < b)
        case InfixOp.Gt => V.fromBool(a > b)
        case InfixOp.Geq => V.fromBool(a >= b)
        case InfixOp.Leq => V.fromBool(a <= b)
        case InfixOp.Neq => V.fromBool(a != b)
      }
    }
  }

}
