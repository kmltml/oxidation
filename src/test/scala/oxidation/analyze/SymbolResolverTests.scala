package oxidation
package analyze

import utest._

object SymbolResolverTests extends TestSuite {

  import SymbolResolver._
  import parse.ast._

  implicit def unresolvedSymbol(s: scala.Symbol): Symbol = Symbol.Unresolved(s.name)
  def g(path: scala.Symbol*): Symbol = Symbol.Global(path.map(_.name))
  def l(s: scala.Symbol): Symbol = Symbol.Local(s.name)

  val tests = apply {
    "resolveSymbols" - {
      "mark def's parameters as local, shadowing external bindings" - {
        resolveSymbols(Vector(
          DefDef('foo,
            Some(Seq(Param("bar", TypeName.Named('i64)))),
            Some(TypeName.Named('i32)),
            Var('bar))
        ), BuiltinSymbols.symbols.withTerms('bar)) ==> Right(Vector(
          DefDef(g('foo),
            Some(Seq(Param("bar", TypeName.Named(g('i64))))),
            Some(TypeName.Named(g('i32))),
            Var(l('bar)))
        ))
      }
      "solve nested expressions" - {
        val foo = g('a, 'b, 'foo)
        val bar = g('a, 'b, 'bar)
        val baz = g('a, 'b, 'baz)
        val importAB_ = Import(Seq("a", "b"), ImportSpecifier.All)
        val scope = BuiltinSymbols.symbols.withTerms(foo, bar, baz)
        "InfixAp" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, InfixAp(InfixOp.Add, Var('foo), Var('bar)))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, InfixAp(InfixOp.Add, Var(foo), Var(bar)))
          ))
        }
        "PrefixAp" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, PrefixAp(PrefixOp.Neg, Var('foo)))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, PrefixAp(PrefixOp.Neg, Var(foo)))
          ))
        }
        "App" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, App(Var('foo), Seq(Var('bar), Var('baz))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, App(Var(foo), Seq(Var(bar), Var(baz))))
          ))
        }
        "Select" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, Select(Var('foo), "bar"))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Select(Var(foo), "bar"))
          ))
        }
        "If" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, If(Var('foo), Var('bar), Some(Var('baz))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, If(Var(foo), Var(bar), Some(Var(baz))))
          ))
        }
        "While" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, While(Var('foo), Var('bar)))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, While(Var(foo), Var(bar)))
          ))
        }
        "Assign" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef('a, None, Assign(Var('foo), None, Var('bar)))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Assign(Var(foo), None, Var(bar)))
          ))
        }
      }
      "solve a block expression" - {
        resolveSymbols(Vector(
          Import(Seq("a", "b"), ImportSpecifier.All),
          DefDef('a, None, None, Block(Seq(
            ValDef('foo, None, Var('baz)),
            VarDef('bar, None, IntLit(2)),
            InfixAp(InfixOp.Add, Var('foo), Var('bar))
          )))
        ), BuiltinSymbols.symbols.withTerms(g('a, 'b, 'foo), g('a, 'b, 'bar), g('a, 'b, 'baz))) ==>
          Right(Vector(
            Import(Seq("a", "b"), ImportSpecifier.All),
            DefDef(g('a), None, None, Block(Seq(
              ValDef(l('foo), None, Var(g('a, 'b, 'baz))),
              VarDef(l('bar), None, IntLit(2)),
              InfixAp(InfixOp.Add, Var(l('foo)), Var(l('bar)))
            )))
          ))
      }
      "solve a struct definition" - {
        resolveSymbols(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          StructDef('foo, Some(Seq("A", "B")), Seq(
            StructMemberDef("a", TypeName.Named('A)),
            StructMemberDef("int", TypeName.Named('i32)),
            StructMemberDef("pointer", TypeName.App(TypeName.Named('ptr), Seq(TypeName.Named('B)))),
            StructMemberDef("custom", TypeName.Named('bar))
          ))
        ), BuiltinSymbols.symbols.withTypes(g('A), g('x, 'y, 'B), g('x, 'y, 'bar))) ==> Right(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          StructDef(g('foo), Some(Seq("A", "B")), Seq(
            StructMemberDef("a", TypeName.Named(l('A))),
            StructMemberDef("int", TypeName.Named(g('i32))),
            StructMemberDef("pointer", TypeName.App(TypeName.Named(g('ptr)), Seq(TypeName.Named(l('B))))),
            StructMemberDef("custom", TypeName.Named(g('x, 'y, 'bar)))
          ))
        ))
      }
      "solve a val definition" - {
        resolveSymbols(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          ValDef('foo, Some(TypeName.Named('z)), IntLit(0))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          ValDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0))
        ))
      }
      "solve a var definition" - {
        resolveSymbols(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          VarDef('foo, Some(TypeName.Named('z)), IntLit(0))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(Seq("x", "y"), ImportSpecifier.All),
          VarDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0))
        ))
      }
    }
  }

}
