package oxidation
package codegen
package pass

import ir._
import scala.annotation.tailrec

import cats._
import cats.data._
import cats.implicits._

object ConstantInlining extends Pass {

  val name = "constant-inlining"

  override type F[A] = Reader[Map[Name, Def], A]
  override val F = implicitly[MonadReader[F, Map[Name, Def]]]

  override def extract[A](fa: F[A]) = fa.run(Map.empty)

  override def txDefs(defs: Vector[Def]): F[Vector[Def]] = {
    val defsByName = defs.map(d => d.name -> d).toMap
    F.local(_ => defsByName)(super.txDefs(defs))
  }

  override def onInstruction = {
    case inst @ Inst.Move(dest, Op.Load(Val.GlobalAddr(name), Val.I(0, Type.I64))) =>
      F.reader { defsByName =>
        Vector(defsByName.get(name).collect {
          case Def.TrivialVal(_, v) => Inst.Move(dest, Op.Copy(v))
        }.getOrElse(inst))
      }
  }

}
