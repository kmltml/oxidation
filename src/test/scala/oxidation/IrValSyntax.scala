package oxidation

import oxidation.ir._

trait IrValSyntax {

  protected implicit def vali(i: Int): Val = Val.I(i, ir.Type.I32)
  protected implicit def valr(r: Register): Val = Val.R(r)

  protected def i8(i: Int): Val = Val.I(i, ir.Type.I8)
  protected def i16(i: Int): Val = Val.I(i, ir.Type.I16)
  protected def i32(i: Int): Val = Val.I(i, ir.Type.I32)
  protected def i64(i: Int): Val = Val.I(i, ir.Type.I64)
  protected def u8(i: Int): Val = Val.I(i, ir.Type.U8)
  protected def u16(i: Int): Val = Val.I(i, ir.Type.U16)
  protected def u32(i: Int): Val = Val.I(i, ir.Type.U32)
  protected def u64(i: Int): Val = Val.I(i, ir.Type.U64)

  protected val u0: Val = Val.I(0, ir.Type.U0)

}
