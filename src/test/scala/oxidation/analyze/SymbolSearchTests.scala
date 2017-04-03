package oxidation
package analyze

import utest._
import parse.ast._
import SymbolSearch._

object SymbolSearchTests extends TestSuite with SymbolSyntax {

  val tests = apply {
    "findSymbols" - {
      "symbols in default module" - {
        findSymbols(Vector(
          DefDef(u('main), None, None, Block(Vector.empty)),
          ValDef(u('foo), None, IntLit(2)),
          VarDef(u('bar), None, IntLit(2)),
          StructDef(u('baz), None, Nil),
          EnumDef(u('foobar), None, Nil)
        )) ==> Right(Symbols(
          terms = Map("main" -> Set(g('main)), "foo" -> Set(g('foo)), "bar" -> Set(g('bar))),
          types = Map("baz" -> Set(g('baz)), "foobar" -> Set(g('foobar)))
        ))
      }
      "error on duplicate symbols" - {
        findSymbols(Vector(
          DefDef(u('main), None, None, Block(Vector.empty)),
          ValDef(u('main), None, IntLit(2))
        )).isLeft ==> true
      }
      "symbols in a module" - {
        findSymbols(Vector(
          Module(List("foo", "bar")),
          DefDef(u('main), None, None, Block(Vector.empty))
        )) ==> Right(Symbols.terms(g('foo, 'bar, 'main)))
      }
      "nested modules" - {
        findSymbols(Vector(
          Module(List("foo")),
          Module(List("bar")),
          DefDef(u('main), None, None, Block(Vector.empty))
        )) ==> Right(Symbols.terms(g('foo, 'bar, 'main)))
      }
    }
  }

}
