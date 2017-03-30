package oxidation
package ir
package serialization

object Tag {

  object Option {
    final val None = 0
    final val Some = None + 1
  }

  object Def {
    final val Fun = 0
    final val ExternFun = 1 + Fun
  }

  object ConstantPoolEntry {
    final val Str = 0
  }

  object Inst {
    final val Move  = 0
    final val Do    = 1 + Move
    final val Label = 1 + Do
    final val Flow  = 1 + Label
  }

  object Op {
    final val Arith = 0
    final val Call  = 1 + Arith
    final val Copy  = 1 + Call
    final val Unary = 1 + Copy
    final val Load  = 1 + Unary
    final val Store  = 1 + Load
    final val Widen  = 1 + Store
    final val Garbled  = 1 + Widen
    final val Member = 1 + Garbled
  }

  object Val {
    final val I = 0
    final val R = 1 + I
    final val G = 1 + R
    final val Struct = 1 + G
  }

  object FlowControl {
    final val Goto   = 0
    final val Return = 1 + Goto
    final val Branch = 1 + Return
  }

  object Type {
    final val U0  = 0
    final val U1  = 1 + U0
    final val I8  = 1 + U1
    final val I16 = 1 + I8
    final val I32 = 1 + I16
    final val I64 = 1 + I32
    final val U8  = 1 + I64
    final val U16 = 1 + U8
    final val U32 = 1 + U16
    final val U64 = 1 + U32
    final val Ptr = 1 + U64
    final val Fun = 1 + Ptr
    final val Struct = 1 + Fun
  }

  object InfixOp {
    final val Add    = 0
    final val Sub    = 1 + Add
    final val Div    = 1 + Sub
    final val Mod    = 1 + Div
    final val Mul    = 1 + Mod
    final val Shl    = 1 + Mul
    final val Shr    = 1 + Shl
    final val BitAnd = 1 + Shr
    final val BitOr  = 1 + BitAnd
    final val Xor    = 1 + BitOr
    final val And    = 1 + Xor
    final val Or     = 1 + And
    final val Eq     = 1 + Or
    final val Lt     = 1 + Eq
    final val Gt     = 1 + Lt
    final val Geq    = 1 + Gt
    final val Leq    = 1 + Geq
    final val Neq    = 1 + Leq
  }

  object PrefixOp {
    final val Neg = 0
    final val Not = 1 + Neg
    final val Inv = 1 + Not
  }

  object Name {
    final val Global = 0
    final val Local  = 1 + Global
  }

  object RegisterNamespace {
    final val CodegenReg = 0
    final val StructLoweringReg = 1 + CodegenReg
  }

}
