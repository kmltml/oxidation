package oxidation
package codegen.pass
package ssa

import cats._
import cats.data._
import cats.implicits._

import ir._

import scala.annotation.tailrec

object UnusedRegRemoval extends Pass {

  override val name = "unused-reg-removal"

  override type F[A] = State[Set[Register], A]
  override val F = implicitly[MonadState[F, Set[Register]]]
  override def extract[A](fa: F[A]): A = fa.runA(Set.empty).value

  override val onDef = {
    case fun @ Def.Fun(_, params, _, blocks, _) =>
      val (dependencies, roots) = blocks.foldMap(b => (b.instructions :+ Inst.Flow(b.flow)).collect {
        case Inst.Eval(dest, op) =>
          val roots = op match {
            case _: Op.Call | _: Op.Store | _: Op.ArrStore => op.reads
            case _ => Set.empty[Register]
          }
          (dest.map(_ -> op.reads).toVector, roots)
        case Inst.Flow(flow) => (Vector.empty, flow.reads)
      }.combineAll) match {
        case (deps, roots) => ((deps ++ params.map(_ -> Set.empty)).toMap, roots)
      }
      @tailrec
      def collectReachable(toVisit: List[Register], visited: Set[Register]): Set[Register] = toVisit match {
        case Nil => visited
        case current :: rest if visited(current) => collectReachable(rest, visited)
        case current :: rest =>
          val neighbors = dependencies(current).filter(!visited.contains(_))
          collectReachable(neighbors ++: toVisit, visited + current)
      }
      val used = collectReachable(roots.toList, Set.empty)
      F.set(used).as(Vector(fun))
  }

  override val onInstruction = {
    case inst @ Inst.Move(dest, op) =>
      F.inspect { used =>
        if(used(dest)) Vector(inst)
        else if(op.isPure) Vector.empty
        else Vector(Inst.Do(op))
      }
  }

}
