package oxidation

import oxidation.ir._

trait IrValSyntax {

  protected implicit def vali(i: Int): Val = Val.I(i, ir.Type.I32)
  protected implicit def valr(r: Register): Val = Val.R(r)

}
