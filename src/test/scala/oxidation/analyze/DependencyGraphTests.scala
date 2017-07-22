package oxidation
package analyze

import oxidation.parse.Span
import utest._
import oxidation.{TypeName => T}
import parse.ast._

object DependencyGraphTests extends TestSuite with SymbolSyntax {

  val loc = Span(None, 0, 0)

  val tests = apply {
    "DependencyEntry.build" - {
      "a def with inferred type" - {
        DependencyEntry.build(DefDef(
          g('foo), Some(List(Param("x", T.Named(g('i32))))), None, Block(Vector(
            App(Var(g('mod, 'fun), loc), List(Var(g('mod, 'foo), loc), Var(l('x), loc)), loc)
          ), loc)
        )) ==> DependencyEntry(Set(g('mod, 'fun), g('mod, 'foo)), explicitType = false)
      }
      "a def with specified type" - {
        DependencyEntry.build(ValDef(
          g('foo), Some(T.Named(g('i32))),
          Var(g('mod, 'foo), loc)
        )) ==> DependencyEntry(Set(g('mod, 'foo)), explicitType = true)
      }
    }
    "DependencyGraph.build" - {
      "a few defs" - {
        DependencyGraph.build(Vector(
          Import(List("a", "b"), ImportSpecifier.All),
          Module(List("foo")),
          StructDef(g('foo, 'unit), None, Nil),
          EnumDef(g('foo, 'nothing), None, Nil),
          TypeAliasDef(g('foo, 'int), None, T.Named(g('i32))),
          DefDef(g('foo, 'fun), None, None, Block(Vector(
            App(Var(g('a, 'b, 'f), loc), List(Var(g('foo, 'const), loc), Var(g('foo, 'mut), loc)), loc)
          ), loc)),
          ValDef(g('foo, 'const), Some(T.Named(g('i32))), IntLit(2, loc)),
          VarDef(g('foo, 'mut), None, IntLit(3, loc))
        )) ==> new DependencyGraph(Map(
          g('foo, 'fun) -> DependencyEntry(Set(g('a, 'b, 'f), g('foo, 'const), g('foo, 'mut)), explicitType = false),
          g('foo, 'const) -> DependencyEntry(Set.empty, explicitType = true),
          g('foo, 'mut) -> DependencyEntry(Set.empty, explicitType = false)
        ))
      }
      "Entries for enum constructors" - {
        DependencyGraph.build(Vector(
          EnumDef(g('foo), None, List(
            EnumVariantDef(g('foo, 'a), List(
              StructMemberDef("x", T.Named(g('i32)))
            )),
            EnumVariantDef(g('foo, 'b), List(
              StructMemberDef("x", T.Named(g('i32)))
            ))
          ))
        )) ==> new DependencyGraph(Map(
          g('foo, 'a) -> DependencyEntry(Set.empty, explicitType = true),
          g('foo, 'b) -> DependencyEntry(Set.empty, explicitType = true)
        ))
      }
    }
    "DependencyGraph#prune" - {
      DependencyGraph(Map(
        g('a) -> DependencyEntry(Set(g('b), g('c), g('d)), explicitType = true),
        g('b) -> DependencyEntry(Set(g('a)), explicitType = true),
        g('c) -> DependencyEntry(Set(g('a), g('b), g('d)), explicitType = false),
        g('d) -> DependencyEntry(Set(g('a), g('b)), explicitType = false)
      )).prune ==> DependencyGraph(Map(
        g('a) -> DependencyEntry(Set(g('c), g('d)), explicitType = true),
        g('b) -> DependencyEntry(Set(), explicitType = true),
        g('c) -> DependencyEntry(Set(g('d)), explicitType = false),
        g('d) -> DependencyEntry(Set(), explicitType = false)
      )).prune
    }
  }

}
