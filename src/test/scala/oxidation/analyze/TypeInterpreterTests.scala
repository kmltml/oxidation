package oxidation
package analyze

import utest._
import TypeInterpreter._
import oxidation.analyze.Type._
import parse.ast._

object TypeInterpreterTests extends TestSuite with SymbolSyntax {

  val tests = apply {
    "solveTree" - {
      val ctxt = Ctxt.default
      "a single type alias" - {
        solveTree(Vector(
          TypeAliasDef(g('int), None, TypeName.Named(g('i32)))
        ), ctxt) ==> Right(ctxt.withTypes(Map(g('int) -> I32)))
      }
      "a type alias depending on another one" - {
        solveTree(Vector(
          TypeAliasDef(g('char), None, TypeName.Named(g('int))),
          TypeAliasDef(g('int), None, TypeName.Named(g('i32)))
        ), ctxt) ==> Right(ctxt.withTypes(Map(g('int) -> I32, g('char) -> I32)))
      }
      "a struct def" - {
        val Right(c) = solveTree(Vector(
          StructDef(g('foo), None, List(
            StructMemberDef("x", TypeName.Named(g('i32))),
            StructMemberDef("y", TypeName.Named(g('u1)))
          ))
        ), ctxt)
        val foo = c.types(g('foo)).asInstanceOf[Struct]
        foo.name ==> g('foo)
        foo.members ==> List(
          StructMember("x", I32),
          StructMember("y", U1)
        )
      }
      "a recursive struct def" - {
        val Right(c) = solveTree(Vector(
          StructDef(g('List), None, List(
            StructMemberDef("head", TypeName.Named(g('i32))),
            StructMemberDef("tail", TypeName.ptr(TypeName.Named(g('List))))
          ))
        ), ctxt)
        val list = c.types(g('List)).asInstanceOf[Struct]
        list.members(0) ==> StructMember("head", I32)
        list.members(1) match {
          case StructMember(name, Type.App(BuiltinSymbols.PtrCons, List(pointee))) =>
            name ==> "tail"
            assert(pointee eq list)
        }
      }
      "EnumDef" - {
        val Right(c) = solveTree(Vector(
          EnumDef(g('foo), None, List(
            EnumVariantDef(g('foo, 'x), List(
              StructMemberDef("a", TypeName.Named(g('i32)))
            )),
            EnumVariantDef(g('foo, 'y), List(
              StructMemberDef("a", TypeName.Named(g('i64)))
            ))
          ))
        ), ctxt)
        val foo = c.types(g('foo)).asInstanceOf[Enum]
        val foo_x = EnumVariant(g('foo, 'x), List(StructMember("a", I32)))
        val foo_y = EnumVariant(g('foo, 'y), List(StructMember("a", I64)))
        foo.variants(0) ==> foo_x
        foo.variants(1) ==> foo_y
        c.terms ==> Map(
          g('foo, 'x) -> Ctxt.Immutable(EnumConstructor(foo, foo_x)),
          g('foo, 'y) -> Ctxt.Immutable(EnumConstructor(foo, foo_y))
        )
      }
      "recursive EnumDef" - {
        val Right(c) = solveTree(Vector(
          EnumDef(g('List), None, List(
            EnumVariantDef(g('List, 'Cons), List(
              StructMemberDef("head", TypeName.Named(g('i32))),
              StructMemberDef("tail", TypeName.ptr(TypeName.Named(g('List))))
            )),
            EnumVariantDef(g('List, 'Nil), Nil)
          ))
        ), ctxt)
        val list = c.types(g('List)).asInstanceOf[Enum]
        list.variants(0) ==> EnumVariant(g('List, 'Cons), List(
          StructMember("head", I32), StructMember("tail", Type.App.ptr(list))))
        list.variants(1) ==> EnumVariant(g('List, 'Nil), Nil)
      }
    }
  }

}
