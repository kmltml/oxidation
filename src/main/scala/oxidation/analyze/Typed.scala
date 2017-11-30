package oxidation
package analyze

case class Typed[+E](expr: E, typ: Type)

object ReprTyped {

  def unapply[A](t: Typed[A]): Some[(A, analyze.Type)] = Some((t.expr, t.typ.repr))

}
