package oxidation
package analyze

import oxidation.parse.Span
import utest._

object SymbolResolverTests extends TestSuite with SymbolSyntax with MatchCaseSyntax {

  import SymbolResolver._
  import parse.ast._

  val loc = Span(None, 0, 0)

  val tests = apply {
    "resolveSymbols" - {
      "mark def's parameters as local, shadowing external bindings" - {
        resolveSymbols(Vector(
          DefDef(u('foo),
            Some(List(Param("bar", TypeName.Named(u('i64))))),
            Some(TypeName.Named(u('i32))),
            Var(u('bar), loc))
        ), BuiltinSymbols.symbols.withTerms(u('bar))) ==> Right(Vector(
          DefDef(g('foo),
            Some(List(Param("bar", TypeName.Named(g('i64))))),
            Some(TypeName.Named(g('i32))),
            Var(l('bar), loc))
        ))
      }
      "find a fully-qualified name" - {
        resolveSymbols(Vector(
          ValDef(u('foo), None, Select(Var(u('module), loc), "member", loc))
        ), BuiltinSymbols.symbols.withTerms(g('module, 'member))) ==> Right(Vector(
          ValDef(g('foo), None, Var(g('module, 'member), loc))
        ))
      }
      "solve partially imported modules" - {
        resolveSymbols(Vector(
          Import(List("a"), ImportSpecifier.Members(List("b"))),
          ValDef(u('foo), None, Select(Var(u('b), loc), "x", loc))
        ), BuiltinSymbols.symbols.withTerms(g('a, 'b, 'x))) ==> Right(Vector(
          Import(List("a"), ImportSpecifier.Members(List("b"))),
          ValDef(g('foo), None, Var(g('a, 'b, 'x), loc))
        ))
      }
      "solve modules imported partially by wildcard" - {
        resolveSymbols(Vector(
          Import(List("a"), ImportSpecifier.All),
          ValDef(u('foo), None, Select(Var(u('b), loc), "x", loc))
        ), BuiltinSymbols.symbols.withTerms(g('a, 'b, 'x))) ==> Right(Vector(
          Import(List("a"), ImportSpecifier.All),
          ValDef(g('foo), None, Var(g('a, 'b, 'x), loc))
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
            ValDef(u('a), None, InfixAp(InfixOp.Add, Var(u('foo), loc), Var(u('bar), loc), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, InfixAp(InfixOp.Add, Var(foo, loc), Var(bar, loc), loc))
          ))
        }
        "PrefixAp" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, PrefixAp(PrefixOp.Neg, Var(u('foo), loc), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, PrefixAp(PrefixOp.Neg, Var(foo, loc), loc))
          ))
        }
        "App" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, App(Var(u('foo), loc), List(Var(u('bar), loc), Var(u('baz), loc)), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, App(Var(foo, loc), List(Var(bar, loc), Var(baz, loc)), loc))
          ))
        }
        "Select" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, Select(Var(u('foo), loc), "bar", loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Select(Var(foo, loc), "bar", loc))
          ))
        }
        "If" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, If(Var(u('foo), loc), Var(u('bar), loc), Some(Var(u('baz), loc)), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, If(Var(foo, loc), Var(bar, loc), Some(Var(baz, loc)), loc))
          ))
        }
        "While" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, While(Var(u('foo), loc), Var(u('bar), loc), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, While(Var(foo, loc), Var(bar, loc), loc))
          ))
        }
        "Assign" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, Assign(Var(u('foo), loc), None, Var(u('bar), loc), loc))
          ), scope) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, Assign(Var(foo, loc), None, Var(bar, loc), loc))
          ))
        }
        "StructLit" - {
          resolveSymbols(Vector(
            importAB_,
            ValDef(u('a), None, StructLit(u('Vec2), List(
              "x" -> Var(u('foo), loc), "y" -> Var(u('bar), loc)
            ), loc))
          ), scope.withTypes(g('a, 'b, 'Vec2))) ==> Right(Vector(
            importAB_,
            ValDef(g('a), None, StructLit(g('a, 'b, 'Vec2), List(
              "x" -> Var(foo, loc), "y" -> Var(bar, loc)
            ), loc))
          ))
        }
      }
      "solve a block expression" - {
        resolveSymbols(Vector(
          Import(List("a", "b"), ImportSpecifier.All),
          DefDef(u('a), None, None, Block(Vector(
            ValDef(u('foo), None, Var(u('baz), loc)),
            VarDef(u('bar), None, IntLit(2, loc)),
            InfixAp(InfixOp.Add, Var(u('foo), loc), Var(u('bar), loc), loc)
          ), loc))
        ), BuiltinSymbols.symbols.withTerms(g('a, 'b, 'foo), g('a, 'b, 'bar), g('a, 'b, 'baz))) ==>
          Right(Vector(
            Import(List("a", "b"), ImportSpecifier.All),
            DefDef(g('a), None, None, Block(Vector(
              ValDef(l('foo), None, Var(g('a, 'b, 'baz), loc)),
              VarDef(l('bar), None, IntLit(2, loc)),
              InfixAp(InfixOp.Add, Var(l('foo), loc), Var(l('bar), loc), loc)
            ), loc))
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
          ValDef(u('foo), Some(TypeName.Named(u('z))), IntLit(0, loc))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          ValDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0, loc))
        ))
      }
      "solve a var definition" - {
        resolveSymbols(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          VarDef(u('foo), Some(TypeName.Named(u('z))), IntLit(0, loc))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z))) ==> Right(Vector(
          Import(List("x", "y"), ImportSpecifier.All),
          VarDef(g('foo), Some(TypeName.Named(g('x, 'y, 'z))), IntLit(0, loc))
        ))
      }
      "EnumDef" - {
        resolveSymbols(Vector(
          Module("x" :: Nil),
          EnumDef(u('foo), None, List(
            EnumVariantDef(u('a), Nil),
            EnumVariantDef(u('b), Nil)
          ))
        ), BuiltinSymbols.symbols) ==> Right(Vector(
          Module("x" :: Nil),
          EnumDef(g('x, 'foo), None, List(
            EnumVariantDef(g('x, 'foo, 'a), Nil),
            EnumVariantDef(g('x, 'foo, 'b), Nil)
          ))
        ))
      }
      "solve patterns with paths" - {
        resolveSymbols(Vector(
          ValDef(u('foo), None, Match(UnitLit(loc), List(
            Pattern.Struct(Some(u('x, 'y, 'z, 'struct)), Nil, ignoreExtra = true, loc) -> UnitLit(loc),
            Pattern.Struct(Some(u('enum, 'variant)), Nil, ignoreExtra = true, loc) -> UnitLit(loc),
            Pattern.Var(u('List, 'Nil), loc) -> UnitLit(loc)
          ), loc))
        ), BuiltinSymbols.symbols.withTypes(g('x, 'y, 'z, 'struct)).withTerms(g('enum, 'variant), g('List, 'Nil))) ==> Right(Vector(
          ValDef(g('foo), None, Match(UnitLit(loc), List(
            Pattern.Struct(Some(g('x, 'y, 'z, 'struct)), Nil, ignoreExtra = true, loc) -> UnitLit(loc),
            Pattern.Struct(Some(g('enum, 'variant)), Nil, ignoreExtra = true, loc) -> UnitLit(loc),
            Pattern.Var(g('List, 'Nil), loc) -> UnitLit(loc)
          ), loc))
        ))
      }
    }
  }

}
