package oxidation

sealed trait TypeName

object TypeName {

  final case class Named(name: Symbol) extends TypeName
  final case class App(const: TypeName, params: Seq[TypeName]) extends TypeName

}