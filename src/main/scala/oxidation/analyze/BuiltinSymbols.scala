package oxidation
package analyze

import Type._

object BuiltinSymbols {

  val typeNames: Map[String, Type] = Map(
    "i8" -> I8, "i16" -> I16, "i32" -> I32, "i64" -> I64,
    "u0" -> U0, "u1" -> U1, "u8" -> U8, "u16" -> U16, "u32" -> U32, "u64" -> U64,
    "ptr" -> U64, "arr" -> U64, "str" -> U64)
  val types: Map[Symbol, Type] = typeNames.map { case (s, v) => Symbol.Global(Seq(s)) -> v }

  val symbols: Symbols = Symbols.types(types.keys.toSeq: _*)

}
