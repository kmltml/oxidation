package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

class Symbols(val types: Multimap[String, Symbol], val terms: Multimap[String, Symbol]) {

  def withTypes(t: (String, Symbol)*): Symbols =
    new Symbols(types = types |+| t.toMap.mapValues(Set(_)), terms = terms)

  def withTerms(t: (String, Symbol)*): Symbols =
    new Symbols(terms = terms |+| t.toMap.mapValues(Set(_)), types = types)

  def |+|(b: Symbols): Symbols =
    new Symbols(terms = terms |+| b.terms, types = types |+| b.types)

}

object Symbols {

  val empty = new Symbols(Map.empty, Map.empty)

  def types(t: (String, Symbol)*): Symbols = empty.withTypes(t: _*)
  def terms(t: (String, Symbol)*): Symbols = empty.withTerms(t: _*)

  implicit val monoidInstance = new Monoid[Symbols] {

    override def empty: Symbols = Symbols.empty

    override def combine(x: Symbols, y: Symbols): Symbols = x |+| y

  }

}
