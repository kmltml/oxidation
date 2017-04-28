package oxidation
package codegen
package pass

import ir._

object ConstantRemoval extends IdPass {

  override def name: String = "constant-removal"

  override def onInstruction = {
    case Inst.Move(dest, Op.Binary(op, Val.I(left, _), Val.I(right, _))) =>
      def fromBool(b: Boolean): Int = if(b) 1 else 0
      val v = op match {
        case InfixOp.Add => left + right
        case InfixOp.Sub => left - right
        case InfixOp.Div => left / right
        case InfixOp.Mod => left % right
        case InfixOp.Mul => left * right
        case InfixOp.Shl => left << right
        case InfixOp.Shr => dest.typ match {
          case Type.U(_) => left >>> right
          case Type.I(_) => left >> right
        }
        case InfixOp.BitAnd => left & right
        case InfixOp.BitOr => left | right
        case InfixOp.Xor => left ^ right
        case InfixOp.Eq => fromBool(left == right)
        case InfixOp.Lt => fromBool(left < right)
        case InfixOp.Gt => fromBool(left > right)
        case InfixOp.Geq => fromBool(left >= right)
        case InfixOp.Leq => fromBool(left <= right)
        case InfixOp.Neq => fromBool(left != right)
      }
      Vector(Inst.Move(dest, Op.Copy(Val.I(v, dest.typ))))

    case Inst.Move(dest, Op.Unary(op, Val.I(src, _))) =>
      val v = op match {
        case PrefixOp.Not => src match {
          case 0 => 1
          case 1 => 0
        }
        case PrefixOp.Neg => -src
        case PrefixOp.Inv => ~src
      }
      Vector(Inst.Move(dest, Op.Copy(Val.I(v, dest.typ))))
  }

  override def onFlow = {
    case FlowControl.Branch(Val.I(0, _), _, lbl) => (Vector.empty, FlowControl.Goto(lbl))
    case FlowControl.Branch(Val.I(1, _), lbl, _) => (Vector.empty, FlowControl.Goto(lbl))
  }

}
