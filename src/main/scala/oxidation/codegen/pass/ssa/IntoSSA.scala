package oxidation
package codegen.pass
package ssa

import ir._
import codegen.Name
import backend.shared.FlowGraph

import cats._
import cats.data._
import cats.implicits._

import monocle.Lens
import scala.collection.mutable

object IntoSSA extends Pass {

  override val name = "into-ssa"

  override type F[A] = Reader[Map[Register, Register], A]
  override val F = implicitly[MonadReader[F, Map[Register, Register]]]

  override def extract[A](fa: F[A]): A = fa.run(Map.empty)

  object SSAReg extends RegisterNamespace {
    override def prefix = "ssa"
  }

  def register(index: Int, typ: Type): Register =
    Register(SSAReg, index, typ)

  def genReg[M[_]](t: Type)(implicit M: MonadState[M, S]): M[Register] =
    for {
      i <- M.inspect(_.nextReg)
      _ <- M.modify(s => s.copy(nextReg = s.nextReg + 1))
    } yield register(i, t)

  case class S(nextReg: Int = 0, writtenRegs: Set[Register] = Set.empty)

  def rename[M[_]](register: Register, forceFresh: Boolean = false)(implicit M: MonadState[M, S]): M[Register] =
    for {
      fresh <- M.inspect(_.writtenRegs(register)).map(_ | forceFresh)
      r <-
        if(fresh) genReg(register.typ)
        else M.modify(s => s.copy(writtenRegs = s.writtenRegs + register)).as(register)
    } yield r

  override val onDef = {
    case d @ Def.Fun(_, params, _, blocks, _) =>
      val flowGraph = FlowGraph(blocks)

      val doms = dominators(flowGraph)
      val dominanceOrder = preorder(doms)
      assert(doms.keySet == blocks.map(_.name).toSet)
      assert(dominanceOrder.toSet == blocks.map(_.name).toSet,
        (blocks.map(_.name).toSet diff dominanceOrder.toSet).show)
      val fronts = frontiers(flowGraph, doms)
      val definedRegs = fronts.keys.map(n => n -> flowGraph.blocks(n).writes).toMap

      type M[A] = State[(S, Map[Name, Map[Register, Register]]), A]
      val M = implicitly[MonadState[M, (S, Map[Name, Map[Register, Register]])]]
      implicit val slens: Lens[(S, Map[Name, Map[Register, Register]]), S] = monocle.function.fields.first

      val ((_, finalBindings), newBlocks) = dominanceOrder.traverse { name =>
        val b = flowGraph.blocks(name)
        for {
          binds <- M.inspect(_._2.get(doms(name)))
          res <- transformBlock[M](b, fronts, definedRegs, binds getOrElse Map.empty)
          _ <- M.modify(t => t.copy(_2 = t._2 + (name -> res._2)))
        } yield res._1
      }.run((S(), Map.empty)).value

      val correctedPhis = newBlocks.map(correctPhis(_, finalBindings, flowGraph))
        .map(b => b.name -> b).toMap

      val reordered = blocks.map(b => correctedPhis(b.name))

      F.pure(Vector(d.copy(body = reordered)))
  }

  private def transformBlock[M[_]](b: Block, fronts: Multimap[Name, Name], defs: Map[Name, Set[Register]], binds: Map[Register, Register])
                                  (implicit M: MonadState[M, S]): M[(Block, Map[Register, Register])] = {
    type MM[A] = State[(S, Map[Register, Register]), A]
    val MM = implicitly[MonadState[MM, (S, Map[Register, Register])]]
    implicit val slens: Lens[(S, Map[Register, Register]), S] = monocle.function.fields.first

    val directFrontierBlocks = fronts.collect {
      case (n, set) if set(b.name) => n
    }

    def searchFrontiers(list: List[Name], visited: Set[Name]): Set[Name] = list match {
      case Nil => visited
      case n :: rest if visited(n) => searchFrontiers(rest, visited)
      case n :: rest =>
        val nsFrontiers = fronts.collect {
          case (name, set) if set(n) => name
        }.toList
        searchFrontiers(nsFrontiers ++: rest, visited + n)
    }
    val frontierBlocks = searchFrontiers(directFrontierBlocks.toList, Set.empty)
    val res = for {
      phis <- frontierBlocks.flatMap(defs).toVector.distinct
      .traverse { r =>
        for {
          renamed <- rename[MM](r, forceFresh = true)
          _ <- MM.modify(t => t.copy(_2 = t._2 + (r -> renamed)))
        } yield Inst.Move(renamed, Op.Phi(Map(b.name -> r)))
      }
      insts <- b.instructions.traverse {
        case inst @ Inst.Eval(dest, _) =>
          for {
            before <- MM.inspect(_._2)
            Vector(Inst.Eval(_, op)) = txInstruction(inst).run(before)
            newDest <- dest.traverse(original => rename[MM](original)
              .flatMap(renamed => MM.modify(t => t.copy(_2 = t._2 + (original -> renamed))).as(renamed)))
          } yield Inst.Eval(newDest, op)
      }
      flow <- MM.inspect{ case (_, bs) => txFlow(b.flow).run(bs) }
      _ = assert(flow._1.isEmpty)
    } yield b.copy(instructions = phis ++ insts, flow = flow._2)
    for {
      s <- M.get
      ((sn, bs), block) = res.run((s, binds)).value
      _ <- M.set(sn)
    } yield (block, bs)
  }

  def correctPhis(block: Block, finalBindings: Map[Name, Map[Register, Register]], flowGraph: FlowGraph): Block = {
    val parents = flowGraph.parents(block.name)
    val insts = block.instructions.flatMap {
      case Inst.Move(dest, Op.Phi(srcs)) if srcs.size == 1 =>
        val src = srcs.values.head
        val reachingDefs = parents.toList
          .traverse(p => finalBindings(p).get(src).map(p -> _))
        reachingDefs.map { rd =>
          Inst.Move(dest, Op.Phi(rd.toMap))
        }

      case i => Vector(i)
    }
    block.copy(instructions = insts)
  }

  override val onVal = {
    case Val.R(r) =>
      F.reader(m => (Vector.empty, Val.R(m.getOrElse(r, r))))
  }

  private[ssa] def dominators(graph: FlowGraph): Map[Name, Name] = {
    val entry = 0
    val order = graph.postorder.reverse
    val Undefined = -1
    val doms = Array.fill[Int](order.size)(Undefined)
    doms(entry) = entry
    val parents = order.map(graph.parents(_).map(order.indexOf))
    var changed = true
    while(changed) {
      changed = false
      for(i <- 1 until (order.size)) {
        val processedParents = parents(i).filter(doms(_) != Undefined)
        val newIdom = processedParents.reduceLeft(intersect)
        if(doms(i) != newIdom) {
          changed = true
          doms(i) = newIdom
        }
      }
    }
    def intersect(a: Int, b: Int): Int =
      if(a == b) a
      else if(a > b) intersect(doms(a), b)
      else intersect(a, doms(b))

    doms.zipWithIndex.toSeq.map { case (d, i) => order(i) -> order(d) }.toMap
  }

  private[ssa] def preorder(doms: Map[Name, Name]): Vector[Name] = {
    def follow(from: Name, acc: List[Name], visited: Set[Name]): List[Name] = {
      val idom = doms(from)
      if(visited(idom)) from :: acc
      else if(idom == from) from :: acc
      else follow(idom, from :: acc, visited)
    }
    def go(keys: Vector[Name], acc: Vector[Name]): Vector[Name] = {
      keys match {
        case first +: rest =>
          val path = follow(first, Nil, acc.toSet).toVector
          go(rest diff path, acc ++ path)
        case _ => acc
      }
    }
    go(doms.keys.toVector, Vector.empty)
  }

  private[ssa] def frontiers(graph: FlowGraph, doms: Map[Name, Name]): Multimap[Name, Name] = {
    var ret: Multimap[Name, Name] = Map.empty
    for(b <- graph.blocks.values) {
      val parents = graph.parents(b.name)
      val idom = doms(b.name)
      if(parents.size >= 2) {
        for(p <- parents) {
          var runner = p
          while(runner != idom && runner != b.name) {
            ret |+|= Map(runner -> Set(b.name))
            runner = doms(runner)
          }
        }
      }
    }
    ret
  }

}
