package oxidation

import oxidation.Symbol.Global

sealed trait TypeName

object TypeName {

  final case class Named(name: Symbol) extends TypeName
  final case class App(const: TypeName, params: List[TypeName]) extends TypeName


  def ptr(pointee: TypeName): TypeName =
    App(Named(Symbol.Global(List("ptr"))), List(pointee))

}