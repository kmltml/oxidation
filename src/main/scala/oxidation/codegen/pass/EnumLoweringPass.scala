package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._

import ir._

object EnumLoweringPass extends Pass {

  val name = "enum-lowering"

  object EnumLoweringReg extends RegisterNamespace {
    val prefix = "el"
  }

  final case class S(nextReg: Int = 0)

  type F[A] = State[S, A]
  val F: MonadState[F, S] = implicitly

  def extract[A](f: F[A]): A = f.runA(S()).value

  def register(i: Int, t: Type): Register =
    Register(EnumLoweringReg, i, t)

  def genReg(t: Type): F[Register] =
    State { case S(nextReg) => (S(nextReg + 1), register(nextReg, t)) }

  private def unpack(loc: PackedLoc, src: Val, tpe: Type): F[(Vector[Inst], Val)] = loc match {
    case PackedAtom(member, shift) =>
      for {
        t1 <- genReg(Type.U64)
        t2 <- genReg(Type.U64)
        conv <- tpe match {
          case Type.Ptr | Type.I64 | Type.F64 =>
            for {
              r <- genReg(tpe)
            } yield (Vector(
              Inst.Move(r, Op.Reinterpret(Val.R(t2), tpe))
            ), Val.R(r))
          case Type.U64 => F.pure((Vector.empty, Val.R(t2)))
          case Type.F32 =>
            for {
              trimmed <- genReg(Type.U32)
              res <- genReg(Type.F32)
            } yield (Vector(
              Inst.Move(trimmed, Op.Trim(Val.R(t2))),
              Inst.Move(res, Op.Reinterpret(Val.R(trimmed), Type.F32))
            ), Val.R(res))
          case tpe: Type.Integral
            if tpe.w < 64 =>

            for {
              r <- genReg(tpe)
            } yield (Vector(
              Inst.Move(r, Op.Trim(Val.R(t2)))
            ), Val.R(r))
        }
      } yield (Vector(
        Inst.Move(t1, Op.Member(src, member)),
        Inst.Move(t2, Op.Binary(InfixOp.Shr, Val.R(t1), Val.I(shift * 8, Type.U64)))
      ) ++ conv._1, conv._2)
    case PackedStruct(members) =>
      val Type.Struct(memberTypes) = tpe
      for {
        traversed <- (members zip memberTypes).traverse {
          case (loc, tpe) => unpack(loc, src, tpe)
        }
        insts = traversed.flatMap(_._1)
        v = Val.Struct(traversed.map(_._2))
      } yield (insts, v)
  }

  private def pack(loc: PackedLoc, src: Val): F[(Vector[Inst], Map[Int, List[Val]])] = loc match {
    case PackedAtom(member, shift) =>
      for {
        conv <- src.typ match {
          case Type.U64 => F.pure((Vector.empty, src))
          case Type.Ptr | Type.I64 | Type.F64 =>
            for {
              r <- genReg(Type.U64)
            } yield (Vector(
              Inst.Move(r, Op.Reinterpret(src, Type.U64))
            ), Val.R(r))

          case Type.F32 =>
            for {
              reinterpreted <- genReg(Type.U32)
              widened <- genReg(Type.U64)
            } yield (Vector(
              Inst.Move(reinterpreted, Op.Reinterpret(src, Type.U32)),
              Inst.Move(widened, Op.Widen(Val.R(reinterpreted)))
            ), Val.R(widened))

          case typ: Type.Integral
            if typ.w < 64 =>

            for {
              r <- genReg(Type.U64)
            } yield (Vector(
              Inst.Move(r, Op.Widen(src))
            ), Val.R(r))

        }
        shifted <- genReg(Type.U64)
      } yield (
        conv._1 :+ Inst.Move(shifted, Op.Binary(InfixOp.Shl, conv._2, Val.I(shift * 8, Type.U64))),
        Map(member -> List(Val.R(shifted)))
      )

    case PackedStruct(members) =>
      val memberTypes = src match {
        case Val(_, Type.Struct(ms)) => ms
        case Val.Enum(tag, _, Type.Enum(variants)) => variants(tag).members
      }
      val memberSelect: Int => F[(Vector[Inst], Val)] = src match {
        case Val.Struct(members) => members.map(v => F.pure((Vector.empty[Inst], v)))
        case Val.Enum(_, members, _) => members.map(v => F.pure((Vector.empty[Inst], v)))
        case Val(_, Type.Struct(memberTypes)) => { i =>
          genReg(memberTypes(i))
            .map(r => (Vector(Inst.Move(r, Op.Member(src, i))), Val.R(r)))
        }
      }
      for {
        traversed <- (members zip memberTypes).zipWithIndex.traverse { case ((memberLoc, typ), index) =>
          for {
            selected <- memberSelect(index)
            packed <- pack(memberLoc, selected._2)
          } yield (selected._1 ++ packed._1, packed._2)
        }
        insts = traversed.flatMap(_._1)
        collected = traversed.foldMap(_._2)
      } yield (insts, collected)
  }

  override val onDef = {
    case fun @ Def.Fun(_, params, ret, _, _) =>
      F.pure(Vector(fun.copy(
        params = params.map(r => r.copy(typ = txType(r.typ))),
        ret = txType(ret)
      ): Def))
  }

  override val onInstruction = {
    case Inst.Move(dest, Op.TagOf(Val(src, e: Type.Enum))) =>
      unpack(e.tagLoc, src, dest.typ)
        .map { case (insts, v) => insts :+ Inst.Move(dest, Op.Copy(v)) }

    case Inst.Move(dest, Op.Unpack(Val(src, e: Type.Enum), variant)) =>
      unpack(e.mappings(variant), src, dest.typ)
        .map { case (insts, v) => insts :+ Inst.Move(dest, Op.Copy(v)) }

    case Inst.Move(dest @ Register(_, _, _: Type.Enum), op) =>
      F.pure(Vector(
        Inst.Move(txReg(dest), op)
      ))
  }

  private def txType(t: Type): Type = t match {
    case e: Type.Enum => e.repr
    case Type.Fun(params, ret) =>
      Type.Fun(params.map(txType), txType(ret))
    case Type.Struct(members) => Type.Struct(members.map(txType))
    case Type.Arr(member, elems) => Type.Arr(txType(member), elems)
    case _ => t
  }

  private def txReg(r: Register): Register =
    r.copy(typ = txType(r.typ))

  override val onVal = {
    case Val.R(r) =>
      F.pure((Vector.empty, Val.R(txReg(r))))
    case Val.G(name, t) =>
      F.pure((Vector.empty, Val.G(name, txType(t))))
    case src @ Val.Enum(tag, members, enumType) =>
      for {
        packedTag <- pack(enumType.tagLoc, Val.I(tag, enumType.tagType))
        packedMembers <- pack(enumType.mappings(tag), src)
        toCollect = packedTag._2 |+| packedMembers._2
        memberVals <- enumType.repr.members.zipWithIndex.traverse { case (typ, index) =>
          toCollect.getOrElse(index, Nil) match {
            case Nil => F.pure((Vector.empty[Inst], Val.I(0, typ): Val))
            case v :: vs =>
              vs.foldM((Vector.empty[Inst], v)) { case ((insts, a), b) =>
                genReg(typ)
                  .map(r => (insts :+ Inst.Move(r, Op.Binary(InfixOp.BitOr, a, b)), Val.R(r): Val))
              }
          }
        }
      } yield (
        packedTag._1 ++ packedMembers._1 ++ memberVals.flatMap(_._1),
        Val.Struct(memberVals.map(_._2))
      )
  }

}
