package oxidation

import oxidation.Symbol.Global

sealed trait TypeName

object TypeName {

  final case class Named(name: Symbol) extends TypeName
  final case class App(const: TypeName, params: List[TypeName]) extends TypeName
  final case class IntLiteral(value: Long) extends TypeName

  def ptr(pointee: TypeName): TypeName =
    App(Named(Symbol.Global(List("ptr"))), List(pointee))

  def arr(member: TypeName, size: Int): TypeName =
    App(Named(Symbol.Global(List("arr"))), List(member, IntLiteral(size)))

}
