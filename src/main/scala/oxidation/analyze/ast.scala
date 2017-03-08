package oxidation
package analyze

/**
  * Created by Kamil on 24.02.2017.
  */
object ast extends Ast {

  override type Typed[+E] = analyze.Typed[E]

}
