package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

object SymbolSearch {

  final case class DuplicatedSymbolError(symbol: Symbol, second: parse.ast.Def) extends AnalysisError

  object TypeDef {
    def unapply(d: parse.ast.Def): Option[String] = d match {
      case parse.ast.StructDef(Symbol.Unresolved(name), _, _) => Some(name)
      case parse.ast.EnumDef(Symbol.Unresolved(name), _, _) => Some(name)
      case parse.ast.TypeAliasDef(Symbol.Unresolved(name), _, _) => Some(name)
      case _ => None
    }
  }

  object TermDef {
    def unapply(d: parse.ast.Def): Option[String] = d match {
      case parse.ast.DefDef(Symbol.Unresolved(name), _, _, _) => Some(name)
      case parse.ast.ValDef(Symbol.Unresolved(name), _, _) => Some(name)
      case parse.ast.VarDef(Symbol.Unresolved(name), _, _) => Some(name)
      case _ => None
    }
  }

  def findSymbols(compilationUnit: Vector[parse.ast.TLD]): Either[DuplicatedSymbolError, Symbols] = {
    val pathPrefix = compilationUnit.collect {
      case parse.ast.Module(path) => path
    }.toList.flatten
    def insertOnly(sym: Symbol, set: Set[Symbol]): Option[Set[Symbol]] =
      if(set(sym)) None else Some(set + sym)
    def rec(defs: List[parse.ast.TLD], types: Set[Symbol], terms: Set[Symbol]): Either[DuplicatedSymbolError, Symbols] = defs match {
      case Nil =>
        Symbols(
          types = types.map(s => s.name -> Set(s)).toMap,
          terms = terms.map(s => s.name -> Set(s)).toMap,
          importedModules = Map.empty).asRight
      case parse.ast.Module(_) :: rest => rec(rest, types, terms)
      case parse.ast.Import(_, _) :: rest => rec(rest, types, terms)

      case (d @ parse.ast.EnumDef(Symbol.Unresolved(name), None, variants)) :: rest =>
        val enumPath = pathPrefix :+ name
        val variantSymbols = variants.map {
          case EnumVariantDef(Symbol.Unresolved(name), _) => Symbol.Global(enumPath :+ name)
        }
        insertOnly(Symbol.Global(enumPath), types).toRight(DuplicatedSymbolError(Symbol.Global(enumPath), d))
          .flatMap(rec(rest, _, terms ++ variantSymbols))


      case (d @ TypeDef(name)) :: rest =>
        val sym = Symbol.Global(pathPrefix :+ name)
        insertOnly(sym, types)
          .toRight(DuplicatedSymbolError(sym, d))
          .flatMap(t => rec(rest, t, terms))
      case (d @ TermDef(name)) :: rest =>
        val sym = Symbol.Global(pathPrefix :+ name)
        insertOnly(sym, terms)
          .toRight(DuplicatedSymbolError(sym, d))
          .flatMap(t => rec(rest, types, t))
    }
    rec(compilationUnit.toList, Set.empty, Set.empty)
  }

}
