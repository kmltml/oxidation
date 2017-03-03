package oxidation
package analyze

object BuiltinSymbols {

  val typeNames = Set("i8", "i16", "i32", "i64", "u0", "u1", "u8", "u16", "u32", "u64", "ptr")
  val types: Set[Symbol] = typeNames.map(s => Symbol.Global(Seq(s)))

  val symbols: Symbols = Symbols.types(types.toSeq: _*)

}
