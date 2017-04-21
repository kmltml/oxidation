package oxidation
package codegen.pass

import cats._
import cats.data._
import cats.implicits._

import ir._

object ArrayDealiasing extends Pass {

  def name = "array-dealiasing"

  type F[A] = State[Map[Register, Register], A]
  val F = implicitly[MonadState[F, Map[Register, Register]]]

  def extract[A](f: F[A]): A = f.runA(Map.empty).value

  def solveAlias(src: Register): F[Register] =
    F.inspect(_.get(src)) flatMap {
      case None => F.pure(src)
      case Some(x) => solveAlias(x)
    }

  override val onInstruction = {
    case Inst.Move(dest, Op.Copy(Val(Val.R(src), _: Type.Arr))) =>
      for {
        bindings <- F.get
        _ = if(bindings.contains(dest)) throw new NotImplementedError("Array moving is not yet implemented")
        alias <- solveAlias(src)
        _ <- F.set(bindings + (dest -> alias))
      } yield Vector.empty

    case Inst.Eval(dest, Op.Elem(Val.R(arr), index)) =>
      solveAlias(arr).map(src => Vector(Inst.Eval(dest, Op.Elem(Val.R(src), index))))

    case Inst.Eval(dest, Op.ArrStore(Val.R(arr), index, value)) =>
      solveAlias(arr).map(arr => Vector(Inst.Eval(dest, Op.ArrStore(Val.R(arr), index, value))))

  }

  override val onDef = {
    case fun: Def.Fun =>
      F.set(Map.empty) as Vector(fun)
  }

}
