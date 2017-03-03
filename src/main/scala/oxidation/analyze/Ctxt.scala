package oxidation
package analyze

class Ctxt(val terms: Map[Symbol, Type], val types: Map[Symbol, Type]) {

  def withTerms(ts: Map[Symbol, Type]): Ctxt = new Ctxt(terms ++ ts, types)

  def withTypes(ts: Map[Symbol, Type]): Ctxt = new Ctxt(terms, types ++ ts)

}

object Ctxt {

  val empty = new Ctxt(Map.empty, Map.empty)

  def terms(ts: (Symbol, Type)*): Ctxt = empty.withTerms(Map(ts: _*))
  def types(ts: (Symbol, Type)*): Ctxt = empty.withTypes(Map(ts: _*))

}