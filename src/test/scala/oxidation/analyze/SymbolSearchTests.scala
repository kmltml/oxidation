package oxidation
package analyze

import utest._
import parse.ast._
import SymbolSearch._

object SymbolSearchTests extends TestSuite {

  def g(path: String*): Symbol = Symbol.Global(path)
  implicit def u(sym: scala.Symbol): Symbol = Symbol.Unresolved(sym.name)

  val tests = apply {
    "findSymbols" - {
      "symbols in default module" - {
        findSymbols(Vector(
          DefDef('main, None, None, Block(Seq())),
          ValDef('foo, None, IntLit(2)),
          VarDef('bar, None, IntLit(2)),
          StructDef('baz, None, Seq.empty),
          EnumDef('foobar, None, Seq.empty)
        )) ==> Right(Symbols(
          terms = Map("main" -> Set(g("main")), "foo" -> Set(g("foo")), "bar" -> Set(g("bar"))),
          types = Map("baz" -> Set(g("baz")), "foobar" -> Set(g("foobar")))
        ))
      }
      "error on duplicate symbols" - {
        findSymbols(Vector(
          DefDef('main, None, None, Block(Seq())),
          ValDef('main, None, IntLit(2))
        )).isLeft ==> true
      }
      "symbols in a module" - {
        findSymbols(Vector(
          Module(Seq("foo", "bar")),
          DefDef('main, None, None, Block(Seq()))
        )) ==> Right(Symbols.terms(g("foo", "bar", "main")))
      }
      "nested modules" - {
        findSymbols(Vector(
          Module(Seq("foo")),
          Module(Seq("bar")),
          DefDef('main, None, None, Block(Seq()))
        )) ==> Right(Symbols.terms(g("foo", "bar", "main")))
      }
    }
  }

}
