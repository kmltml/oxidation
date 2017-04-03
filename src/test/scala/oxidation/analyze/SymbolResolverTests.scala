package oxidation
package analyze

import utest._

object SymbolResolverTests extends TestSuite with SymbolSyntax {

  import SymbolResolver._
  import parse.ast._

  val tests = apply {
    "resolveSymbols" - {
      "mark def's parameters as local, shadowing external bindings" - {
        resolveSymbols(Vector(
          DefDef(u('foo),
            Some(List(Param("bar", TypeName.Named(u('i64))))),
            Some(TypeName.Named(u('i32))),
            Var(u('bar)))
        ), BuiltinSymbols.symbols.withTerms(u('bar))) ==> Right(Vector(
          DefDef(g('foo),
            Some(List(Param("bar", TypeName.Named(g('i64))))),
            Some(TypeName.Named(g('i32))),
            Var(l('bar)))
        ))
      }
      "solve nested expressions" - {
        val foo = g('a, 'b, 'foo)
        val bar = g('a, 'b, 'bar)
        val baz = g('a, 'b, 'baz)
        val importAB_ = Import(List("a", "b"), ImportSpecifier.All)
        val scope = BuiltinSymbols.symbols.withTerms(foo, bar, baz)
        "InfixAp" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, InfixAp(InfixOp.Add, Var(u('foo)), Var(u('bar))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, InfixAp(InfixOp.Add, Var(foo), Var(bar)))
          ))
        }
        "PrefixAp" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, PrefixAp(PrefixOp.Neg, Var(u('foo))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, PrefixAp(PrefixOp.Neg, Var(foo)))
          ))
        }
        "App" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, App(Var(u('foo)), List(Var(u('bar)), Var(u('baz)))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, App(Var(foo), List(Var(bar), Var(baz))))
          ))
        }
        "Select" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, Select(Var(u('foo)), "bar"))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Select(Var(foo), "bar"))
          ))
        }
        "If" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, If(Var(u('foo)), Var(u('bar)), Some(Var(u('baz)))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, If(Var(foo), Var(bar), Some(Var(baz))))
          ))
        }
        "While" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, While(Var(u('foo)), Var(u('bar))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, While(Var(foo), Var(bar)))
          ))
        }
        "Assign" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, Assign(Var(u('foo)), None, Var(u('bar))))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Assign(Var(foo), None, Var(bar)))
          ))
        }
        "StructLit" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, StructLit(u('Vec2), List(
              "x" -> Var(u('foo)), "y" -> Var(u('bar))
            )))
          ), scope.withTypes(g('a, 'b, 'Vec2))) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, StructLit(g('a, 'b, 'Vec2), List(
              "x" -> Var(foo), "y" -> Var(bar)
            )))
          ))
        }
      }
      "solve a block expression" - {
        resolveSymbols(Vector(
          Import(List("a", "b"), ImportSpecifier.All),
          DefDef(u('a), None, None, Block(Vector(
            ValDef(u('foo), None, Var(u('baz))),
            VarDef(u('bar), None, IntLit(2)),
            InfixAp(InfixOp.Add, Var(u('foo)), Var(u('bar)))
          )))
        ), BuiltinSymbols.symbols.withTerms(g('a, 'b, 'foo), g('a, 'b, 'bar), g('a, 'b, 'baz))) ==>
          Right(Vector(
            Import(List("a", "b"), ImportSpecifier.All),
            DefDef(g('a), None, None, Block(Vector(
              ValDef(l('foo), None, Var(g('a, 'b, 'baz))),
              VarDef(l('bar), None, IntLit(2)),
              InfixAp(InfixOp.Add, Var(l('foo)), Var(l('bar)))
            )))
          ))
      }
      "solve a struct definition" - {
        resolveSymbols(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          StructDef(u('foo), Some(List("A", "B")), List(
            StructMemberDef("a", TypeName.Named(u('A))),
            StructMemberDef("int", TypeName.Named(u('i32))),
            StructMemberDef("pointer", TypeName.App(TypeName.Named(u('ptr)), List(TypeName.Named(u('B))))),
            StructMemberDef("custom", TypeName.Named(u('bar)))
          ))
        ), BuiltinSymbols.symbols.withTypes(g('A), g('x, 'y, 'B), g('x, 'y, 'bar))) ==> Right(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          StructDef(g('foo), Some(List("A", "B")), List(
            StructMemberDef("a", TypeName.Named(l('A))),
            StructMemberDef("int", TypeName.Named(g('i32))),
            StructMemberDef("pointer", TypeName.App(TypeName.Named(g('ptr)), List(TypeName.Named(l('B))))),
            StructMemberDef("custom", TypeName.Named(g('x, 'y, 'bar)))
          ))
        ))
      }
      "solve a val definition" - {
        resolveSymbols(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          ValDef(u('foo), Some(TypeName.Named(u('z))), IntLit(0))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          ValDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0))
        ))
      }
      "solve a var definition" - {
        resolveSymbols(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          VarDef(u('foo), Some(TypeName.Named(u('z))), IntLit(0))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          VarDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0))
        ))
      }
    }
  }

}
