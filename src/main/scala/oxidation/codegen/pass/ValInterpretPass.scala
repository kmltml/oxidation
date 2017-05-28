package oxidation
package codegen
package pass

import ir._
import cats._
import cats.data._
import cats.implicits._

import scala.util.control.NonFatal

object ValInterpretPass extends IdPass {

  override def name: String = "val-interpret"

  private def toVal(v: Interpreter.V, typ: Type): Val = v match {
    case Interpreter.V.I(i) => Val.I(i, typ)
    case Interpreter.V.F32(f) => Val.F32(f)
    case Interpreter.V.F64(f) => Val.F64(f)
    case Interpreter.V.S(members) =>
      val struct = typ.asInstanceOf[Type.Struct]
      Val.Struct((members zip struct.members).map((toVal _).tupled))
    case Interpreter.V.A(elems) =>
      val Type.Arr(elem, _) = typ
      Val.Array(elems.map(toVal(_, elem)))
    case Interpreter.V.U0 => Val.I(0, Type.U0)
  }

  override def onDef = {
    case d @ Def.ComputedVal(name, body, typ, _) =>
      try {
        val value = toVal(Interpreter.interpret(body), typ)
        Vector(Def.TrivialVal(name, value))
      } catch {
        case NonFatal(_) => Vector(d)
      }
  }

}
