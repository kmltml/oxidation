package oxidation
package parse


object ast extends Ast {

  override type Typed[+A] = A

  override type TypeInfo = TypeName

  protected def extractTyped[A](t: A): A = t

}
