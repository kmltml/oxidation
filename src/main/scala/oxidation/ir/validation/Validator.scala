package oxidation
package ir
package validation

import oxidation.backend.shared.FlowGraph
import oxidation.codegen.Name

import cats._
import cats.data._
import cats.implicits._

object Validator {

  def validateDef(d: Def): Either[ValidationError, Unit] = d match {
    case Def.Fun(name, params, retTy, blocks, constantPool) =>
      val graph = FlowGraph(blocks)
      val written: Map[Name, Set[Register]] = blocks.map(b => b.name -> b.instructions.flatMap {
        case Inst.Move(dest, _) => Some(dest)
        case _ => None
      }.toSet).toMap
      blocks.traverse_(validateBlock(name, _, written, params.toSet, graph))
    case Def.ExternFun(_, _, _) => Right(())
  }

  def validateBlock(defName: Name, block: Block, writes: Map[Name, Set[Register]], params: Set[Register], graph: FlowGraph): Either[ValidationError, Unit] = {
    val defined: Set[Register] = graph.predecessors(block.name).flatMap(writes) ++ params
    block.instructions.zipWithIndex.traverse_ {
      case (inst, i) => validateInstruction(Location(defName, block.name, i), inst)
    }.value.runA(defined).value
  }

  type ES[A] = EitherT[data.State[Set[Register], ?], ValidationError, A]
  val ES = MonadError[ES, ValidationError]
  val S = new MonadState[ES, Set[Register]] {
    private val m = Monad[ES]
    override def get: ES[Set[Register]] = EitherT.right(State.get)
    override def set(s: Set[Register]): ES[Unit] = EitherT.right(State.set(s))
    override def pure[A](x: A): ES[A] = m.pure(x)
    override def flatMap[A, B](fa: ES[A])(f: (A) => ES[B]): ES[B] = m.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: (A) => ES[Either[A, B]]): ES[B] = m.tailRecM(a)(f)
  }

  def validateInstruction(loc: Location, inst: Inst): ES[Unit] = inst match {
    case Inst.Move(dest, op @ Op.Widen(v)) =>
      for {
        _ <- validateOp(loc, op)
        _ <- cond(v.typ.isInstanceOf[Type.Num], ValidationError.NotANumericType(loc, v.typ))
        _ <- S.modify(_ + dest)
      } yield ()
    case Inst.Move(dest, Op.Garbled) => S.modify(_ + dest)
    case Inst.Move(dest, op) => for {
      opType <- validateOp(loc, op)
      _ <- cond(opType.forall(_ == dest.typ), ValidationError.WrongType(loc, dest.typ, opType.get))
      _ <- S.modify(_ + dest)
    } yield ()

    case Inst.Do(op) => validateOp(loc, op).as(())

    case Inst.Label(_) => ES.pure(())
    case Inst.Flow(_) => ES.pure(())
  }

  def validateOp(loc: Location, op: Op): ES[Option[Type]] = op match {
    case Op.Copy(src) => valType(loc, src).map(Some(_))
    case Op.Widen(v)  => valType(loc, v).as(None)
    case Op.Garbled  => ES.pure(None)
    case Op.Call(fn, params) =>
      def flatten(p: List[Type]): List[Type] = p flatMap {
        case t @ (_: Type.Num | Type.U0 | Type.U1 | Type.Ptr | _: Type.Fun) => List(t)
        case Type.Struct(members) => members.toList
      }
      for {
        fnType <- valType(loc, fn).flatMap {
          case f: Type.Fun => ES.pure(f)
          case t => ES.raiseError[Type.Fun](ValidationError.NotAFunction(loc, t))
        }
        signature = flatten(fnType.params)
        flattenedVals = flatten(params.map(_.typ))
        _ <- cond(signature.size == flattenedVals.size, ValidationError.WrongArity(loc, signature.size, flattenedVals.size))
        _ <- (signature zip flattenedVals).traverse_ {
          case (e, f) =>
            if(e == f) ES.pure(())
            else ES.raiseError[Unit](ValidationError.WrongType(loc, e, f))
        }
      } yield Some(fnType.ret)
    case Op.Member(src, index) =>
      for {
        structType <- valType(loc, src).flatMap {
          case s: Type.Struct => ES.pure(s)
          case t => ES.raiseError[Type.Struct](ValidationError.NotAStruct(loc, t))
        }
        t <- structType.members.lift(index) match {
          case Some(t) => ES.pure(t)
          case None => ES.raiseError[Type.Struct](ValidationError.StructMemberOutOfBounds(loc, structType, index))
        }
      } yield Some(t)
    case Op.Load(addr, offset) =>
      for {
        addrType <- valType(loc, addr)
        _ <- cond(addrType == Type.Ptr, ValidationError.WrongType(loc, Type.Ptr, addrType))
        offsetType <- valType(loc, offset)
        _ <- cond(offsetType == Type.I64, ValidationError.WrongType(loc, Type.I64, offsetType))
      } yield None
    case Op.Store(addr: Val, offset: Val, value: Val) =>
      for {
        addrType <- valType(loc, addr)
        _ <- cond(addrType == Type.Ptr, ValidationError.WrongType(loc, Type.Ptr, addrType))
        offsetType <- valType(loc, offset)
        _ <- cond(offsetType == Type.I64, ValidationError.WrongType(loc, Type.I64, offsetType))
        _ <- valType(loc, value)
      } yield Some(Type.U0)
    case Op.Unary(op: PrefixOp, right: Val)  => op match {
      case PrefixOp.Inv | PrefixOp.Neg => for {
        rightType <- valType(loc, right)
        _ <- cond(rightType.isInstanceOf[Type.Num], ValidationError.NotANumericType(loc, rightType))
      } yield Some(rightType)

      case PrefixOp.Not => for {
        rightType <- valType(loc, right)
        _ <- cond(rightType == Type.U1, ValidationError.WrongType(loc, Type.U1, rightType))
      } yield Some(Type.U1)
    }
    case Op.Arith(InfixOp.Add | InfixOp.Sub | InfixOp.Mul | InfixOp.Div | InfixOp.Mod | InfixOp.Shl | InfixOp.Shr, left, right) =>
      for {
        ltype <- valType(loc, left)
        _ <- cond(ltype.isInstanceOf[Type.Num], ValidationError.NotANumericType(loc, ltype))
        rtype <- valType(loc, right)
        _ <- cond(ltype == rtype, ValidationError.WrongType(loc, ltype, rtype))
      } yield Some(ltype)

    case Op.Arith(InfixOp.Eq | InfixOp.Neq, left, right) =>
      for {
        ltype <- valType(loc, left)
        rtype <- valType(loc, right)
        _ <- cond(ltype == rtype, ValidationError.WrongType(loc, ltype, rtype))
      } yield Some(Type.U1)

    case Op.Arith(InfixOp.Lt | InfixOp.Leq | InfixOp.Gt | InfixOp.Geq, left, right) =>
      for {
        ltype <- valType(loc, left)
        _ <- cond(ltype.isInstanceOf[Type.Num], ValidationError.NotANumericType(loc, ltype))
        rtype <- valType(loc, right)
        _ <- cond(ltype == rtype, ValidationError.WrongType(loc, ltype, rtype))
      } yield Some(Type.U1)
    case Op.Arith(InfixOp.Xor | InfixOp.BitAnd | InfixOp.BitOr, left, right) =>
      for {
        ltype <- valType(loc, left)
        _ <- cond(ltype.isInstanceOf[Type.Num] || ltype == Type.U1, ValidationError.NotANumericType(loc, ltype))
        rtype <- valType(loc, right)
        _ <- cond(ltype == rtype, ValidationError.WrongType(loc, ltype, rtype))
      } yield Some(ltype)
  }

  private def valType(loc: Location, v: Val): EitherT[State[Set[Register], ?], ValidationError, Type] = v match {
    case _: Val.I | _: Val.Const | _: Val.G | _: Val.Struct => v.typ.pure[ES]
    case Val.R(reg) => for {
      defined <- S.inspect(_(reg))
      _ <- cond(defined, ValidationError.UseBeforeDefine(loc, reg): ValidationError)
    } yield reg.typ
  }

  private def cond(test: Boolean, error: => ValidationError): ES[Unit] =
    EitherT.cond(test, (), error)

}