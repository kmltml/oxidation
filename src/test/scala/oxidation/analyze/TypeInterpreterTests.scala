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
        solveTree(Vector(
          StructDef(g('foo), None, Seq(
            StructMemberDef("x", TypeName.Named(g('i32))),
            StructMemberDef("y", TypeName.Named(g('u1)))
          ))
        ), ctxt) ==> Right(ctxt.withTypes(Map(g('foo) -> Struct(g('foo), Seq(
          StructMember("x", I32),
          StructMember("y", U1)
        )))))
      }
    }
  }

}
