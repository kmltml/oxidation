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
          case StructMember(name, Ptr(pointee)) =>
            name ==> "tail"
            assert(pointee eq list)
        }
      }
    }
  }

}
