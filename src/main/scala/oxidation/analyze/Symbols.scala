package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

case class Symbols(types: Multimap[String, Symbol], terms: Multimap[String, Symbol]) {

  def withTypes(t: Symbol*): Symbols =
    Symbols(types = types |+| t.map(s => s.name -> Set(s)).toMap, terms = terms)

  def withTerms(t: Symbol*): Symbols =
    Symbols(terms = terms |+| t.map(s => s.name -> Set(s)).toMap, types = types)

  def |+|(b: Symbols): Symbols =
    Symbols(terms = terms |+| b.terms, types = types |+| b.types)

  def shadowTerm(t: Symbol): Symbols = copy(terms = terms.updated(t.name, Set(t)))
  def shadowType(t: Symbol): Symbols = copy(types = types.updated(t.name, Set(t)))

  def isEmpty: Boolean = types.isEmpty && terms.isEmpty

  def findPrefixed(path: List[String]): Symbols = {
    val newTerms = terms.values.flatten.collect {
      case s @ Symbol.Global(`path` :+ _) => s.name -> Set(s: Symbol)
    }.toMap
    val newTypes = types.values.flatten.collect {
      case s @ Symbol.Global(`path` :+ _) => s.name -> Set(s: Symbol)
    }.toMap
    Symbols(types = newTypes, terms = newTerms)
  }

  def findExact(path: Seq[String]): Symbols = {
    val newTerms = terms.values.flatten.collect {
      case s @ Symbol.Global(`path`) => s.name -> Set(s: Symbol)
    }.toMap
    val newTypes = types.values.flatten.collect {
      case s @ Symbol.Global(`path`) => s.name -> Set(s: Symbol)
    }.toMap
    Symbols(types = newTypes, terms = newTerms)
  }

}

object Symbols {

  val empty = new Symbols(Map.empty, Map.empty)

  def types(t: Symbol*): Symbols = empty.withTypes(t: _*)
  def terms(t: Symbol*): Symbols = empty.withTerms(t: _*)

  implicit val monoidInstance = new Monoid[Symbols] {

    override def empty: Symbols = Symbols.empty

    override def combine(x: Symbols, y: Symbols): Symbols = x |+| y

  }

}
