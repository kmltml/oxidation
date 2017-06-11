package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

case class Symbols(types: Multimap[String, Symbol], terms: Multimap[String, Symbol], importedModules: Multimap[String, List[String]]) {

  def withTypes(t: Symbol*): Symbols =
    copy(types = types |+| t.map(s => s.name -> Set(s)).toMap)

  def withTerms(t: Symbol*): Symbols =
    copy(terms = terms |+| t.map(s => s.name -> Set(s)).toMap)

  def |+|(b: Symbols): Symbols =
    Symbols(terms = terms |+| b.terms, types = types |+| b.types, importedModules = importedModules |+| b.importedModules)

  def shadowTerm(t: Symbol): Symbols = copy(terms = terms.updated(t.name, Set(t)))
  def shadowType(t: Symbol): Symbols = copy(types = types.updated(t.name, Set(t)))

  def isEmpty: Boolean = types.isEmpty && terms.isEmpty && importedModules.isEmpty

  def findPrefixed(path: List[String]): Symbols = {
    val newTerms = terms.values.flatten.collect {
      case s @ Symbol.Global(`path` :+ _) => s.name -> Set(s: Symbol)
    }.toMap
    val newTypes = types.values.flatten.collect {
      case s @ Symbol.Global(`path` :+ _) => s.name -> Set(s: Symbol)
    }.toMap
    val newModules = modules.collect {
      case mod @ (`path` :+ n) => n -> Set(mod)
    }.toMap
    copy(types = newTypes, terms = newTerms, importedModules = newModules)
  }

  def findExact(path: List[String]): Symbols = {
    val newTerms = terms.values.flatten.collect {
      case s @ Symbol.Global(`path`) => s.name -> Set(s: Symbol)
    }.toMap
    val newTypes = types.values.flatten.collect {
      case s @ Symbol.Global(`path`) => s.name -> Set(s: Symbol)
    }.toMap
    val newModules = if(modules.contains(path)) Map(path.last -> Set(path)) else Map.empty[String, Set[List[String]]]
    copy(types = newTypes, terms = newTerms, importedModules = newModules)
  }

  lazy val modules: Set[List[String]] = {
    val symbols = (types.values ++ terms.values).flatten.toSet
    symbols.flatMap {
      case Symbol.Global(path) => path.init.inits
      case Symbol.Unresolved(_) => Seq.empty
      case Symbol.Local(_) => Seq.empty
    }
  }

}

object Symbols {

  val empty = new Symbols(Map.empty, Map.empty, Map.empty)

  def types(t: Symbol*): Symbols = empty.withTypes(t: _*)
  def terms(t: Symbol*): Symbols = empty.withTerms(t: _*)

  implicit val monoidInstance = new Monoid[Symbols] {

    override def empty: Symbols = Symbols.empty

    override def combine(x: Symbols, y: Symbols): Symbols = x |+| y

  }

}
