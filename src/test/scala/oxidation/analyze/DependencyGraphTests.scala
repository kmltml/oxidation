package oxidation
package analyze

import utest._
import parse.ast.{Type => T, _}

object DependencyGraphTests extends TestSuite with SymbolSyntax {

  val tests = apply {
    "DependencyEntry.build" - {
      "a def with inferred type" - {
        DependencyEntry.build(DefDef(
          g('foo), Some(Seq(Param("x", T.Named(g('i32))))), None, Block(Seq(
            App(Var(g('mod, 'fun)), Seq(Var(g('mod, 'foo)), Var(l('x))))
          ))
        )) ==> DependencyEntry(Set(g('mod, 'fun), g('mod, 'foo)), None)
      }
      "a def with specified type" - {
        DependencyEntry.build(ValDef(
          g('foo), Some(T.Named(g('i32))),
          Var(g('mod, 'foo))
        )) ==> DependencyEntry(Set(g('mod, 'foo)), Some(T.Named(g('i32))))
      }
    }
    "DependencyGraph.build" - {
      "a few defs" - {
        DependencyGraph.build(Vector(
          Import(Seq("a", "b"), ImportSpecifier.All),
          Module(Seq("foo")),
          StructDef(g('foo, 'unit), None, Seq.empty),
          EnumDef(g('foo, 'nothing), None, Seq.empty),
          TypeAliasDef(g('foo, 'int), None, T.Named(g('i32))),
          DefDef(g('foo, 'fun), None, None, Block(Seq(
            App(Var(g('a, 'b, 'f)), Seq(Var(g('foo, 'const)), Var(g('foo, 'mut))))
          ))),
          ValDef(g('foo, 'const), Some(T.Named(g('i32))), IntLit(2)),
          VarDef(g('foo, 'mut), None, IntLit(3))
        )) ==> new DependencyGraph(Map(
          g('foo, 'fun) -> DependencyEntry(Set(g('a, 'b, 'f), g('foo, 'const), g('foo, 'mut)), None),
          g('foo, 'const) -> DependencyEntry(Set.empty, Some(T.Named(g('i32)))),
          g('foo, 'mut) -> DependencyEntry(Set.empty, None)
        ))
      }
    }
    "DependencyGraph#prune" - {
      DependencyGraph(Map(
        g('a) -> DependencyEntry(Set(g('b), g('c), g('d)), Some(T.Named(g('i32)))),
        g('b) -> DependencyEntry(Set(g('a)), Some(T.Named(g('i64)))),
        g('c) -> DependencyEntry(Set(g('a), g('b), g('d)), None),
        g('d) -> DependencyEntry(Set(g('a), g('b)), None)
      )).prune ==> DependencyGraph(Map(
        g('a) -> DependencyEntry(Set(g('c), g('d)), Some(T.Named(g('i32)))),
        g('b) -> DependencyEntry(Set(), Some(T.Named(g('i64)))),
        g('c) -> DependencyEntry(Set(g('d)), None),
        g('d) -> DependencyEntry(Set(), None)
      )).prune
    }
  }

}
