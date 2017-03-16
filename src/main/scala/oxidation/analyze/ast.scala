package oxidation
package analyze

object ast extends Ast {

  override type Typed[+E] = analyze.Typed[E]

  override type TypeInfo = Type

}
