package oxidation
package analyze

import utest._
import parse.ast._
import SymbolSearch._
import oxidation.parse.Span

object SymbolSearchTests extends TestSuite with SymbolSyntax {

  val loc = Span(None, 0, 0)

  val tests = apply {
    "findSymbols" - {
      "symbols in default module" - {
        findSymbols(Vector(
          DefDef(u('main), None, None, Block(Vector.empty, loc)),
          ValDef(u('foo), None, IntLit(2, loc)),
          VarDef(u('bar), None, IntLit(2, loc)),
          StructDef(u('baz), None, Nil),
          EnumDef(u('foobar), None, Nil)
        )) ==> Right(Symbols(
          terms = Map("main" -> Set(g('main)), "foo" -> Set(g('foo)), "bar" -> Set(g('bar))),
          types = Map("baz" -> Set(g('baz)), "foobar" -> Set(g('foobar))),
          importedModules = Map.empty
        ))
      }
      "error on duplicate symbols" - {
        findSymbols(Vector(
          DefDef(u('main), None, None, Block(Vector.empty, loc)),
          ValDef(u('main), None, IntLit(2, loc))
        )).isLeft ==> true
      }
      "symbols in a module" - {
        findSymbols(Vector(
          Module(List("foo", "bar")),
          DefDef(u('main), None, None, Block(Vector.empty, loc))
        )) ==> Right(Symbols.terms(g('foo, 'bar, 'main)))
      }
      "nested modules" - {
        findSymbols(Vector(
          Module(List("foo")),
          Module(List("bar")),
          DefDef(u('main), None, None, Block(Vector.empty, loc))
        )) ==> Right(Symbols.terms(g('foo, 'bar, 'main)))
      }
      "enum constructors" - {
        findSymbols(Vector(
          Module("foo" :: Nil),
          EnumDef(u('bar), None, List(
            EnumVariantDef(u('x), Nil),
            EnumVariantDef(u('y), Nil)
          ))
        )) ==> Right(
          Symbols.terms(g('foo, 'bar, 'x), g('foo, 'bar, 'y))
          .withTypes(g('foo, 'bar)))
      }
    }
  }

}
