package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

object SymbolSearch {

  def findSymbols(compilationUnit: Vector[parse.ast.Def]): Symbols = {
    val pathPrefix = Seq()
    compilationUnit.foldMap {
      case parse.ast.StructDef(name, _, _) => Symbols.types(name -> Symbol.Global(pathPrefix :+ name))
      case parse.ast.ValDef(name, _, _) => Symbols.terms(name -> Symbol.Global(pathPrefix :+ name))
      case parse.ast.VarDef(name, _, _) => Symbols.terms(name -> Symbol.Global(pathPrefix :+ name))
      case parse.ast.DefDef(name, _, _, _) => Symbols.terms(name -> Symbol.Global(pathPrefix :+ name))
    }
  }

}
