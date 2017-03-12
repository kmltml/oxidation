package oxidation
package analyze

import parse.{ ast => untyped }

import cats._
import cats.data._
import cats.implicits._

object TypeInterpreter {

  type S[A] = StateT[Either[AnalysisError, ?], Ctxt, A]
  val S = MonadState[S, Ctxt]

  def solveTree(defs: Vector[untyped.TypeDef], ctxt: Ctxt): Either[AnalysisError, Ctxt] = {
    val defsByName = defs.map(d => d.name -> d).toMap
    defs.traverse(solveTypeDef(_, defsByName)).runS(ctxt)
  }

  def solveTypeDef(d: untyped.TypeDef, defs: Map[Symbol, untyped.TypeDef]): S[Type] =
    for {
      ctxt <- S.get
      t <- if(ctxt.types.contains(d.name)) S.pure(ctxt.types(d.name)) else d match {
        case untyped.TypeAliasDef(_, None, body) =>
          for {
            t <- findType(body, defs)
            _ <- solved(d.name, t)
          } yield t
        case untyped.StructDef(_, None, members) =>
          for {
            typedMembers <- members.toVector.traverse {
              case StructMemberDef(name, tpe) => findType(tpe, defs).map(Type.StructMember(name, _))
            }
            struct = Type.Struct(d.name, typedMembers)
            _ <- solved(d.name, struct)
          } yield struct
      }
    } yield t

  private def findType(name: TypeName, defs: Map[Symbol, untyped.TypeDef]): S[Type] = name match {
    case TypeName.Named(s) =>
      for {
        ctxt <- S.get
        t <- if(ctxt.types.contains(s)) S.pure(ctxt.types(s)) else solveTypeDef(defs(s), defs)
      } yield t
  }

  private def solved(name: Symbol, t: Type): S[Unit] = S.modify { ctxt =>
    ctxt.withTypes(Map(name -> t))
  }

}
