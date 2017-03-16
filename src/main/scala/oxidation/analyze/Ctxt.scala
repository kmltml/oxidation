package oxidation
package analyze

case class Ctxt(terms: Map[Symbol, Ctxt.Term], types: Map[Symbol, Type]) {

  def withTerms(ts: Map[Symbol, Ctxt.Term]): Ctxt = new Ctxt(terms ++ ts, types)

  def withTypes(ts: Map[Symbol, Type]): Ctxt = new Ctxt(terms, types ++ ts)

}

object Ctxt {

  sealed trait Term {
    def typ: Type
  }

  final case class Mutable(typ: Type) extends Term
  final case class Immutable(typ: Type) extends Term

  val empty = new Ctxt(Map.empty, Map.empty)

  val default = types(BuiltinSymbols.types.toSeq: _*)

  def terms(ts: (Symbol, Term)*): Ctxt = empty.withTerms(Map(ts: _*))
  def types(ts: (Symbol, Type)*): Ctxt = empty.withTypes(Map(ts: _*))

}
