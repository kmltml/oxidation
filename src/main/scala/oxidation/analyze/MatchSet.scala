package oxidation
package analyze

import cats._
import cats.data._
import cats.implicits._

sealed trait MatchSet {

  import MatchSet._

  def -(p: ast.Pattern): MatchSet = p match {
    case ast.Pattern.Ignore(_) | ast.Pattern.Var(_, _) => Empty
    case ast.Pattern.Alias(_, Typed(pattern, _), _) => this - pattern
    case p if p.isInfinitesimal => this
    case ast.Pattern.BoolLit(patbool, _) => this match {
      case Any(Type.U1) => Bool(!patbool)
      case Bool(`patbool`) => Empty
      case _ => this
    }
    case ast.Pattern.Struct(_, patmembers, _, _) =>
      val sets = this match {
        case Any(Type.Struct(_, members)) => solveProduct(patmembers, members.map(m => (m.name, Any(m.typ))))
        case Struct(ms) => solveProduct(patmembers, ms)
        case Empty => Nil
      }
      sets.map(Struct).foldLeft(Empty: MatchSet)(_ + _)

    case ast.Pattern.Enum(matchedVariant, patternMembers, _, _) =>
      val expanded = this match {
        case Any(Type.Enum(_, variants)) =>
          Enum(variants.map {
            case Type.EnumVariant(name, members) =>
              name.name -> Struct(members.map {
                case Type.StructMember(memberName, typ) => memberName -> Any(typ)
              })
          }.toMap)
        case e: Enum => e
        case Empty => Empty
      }
      expanded match {
        case Empty => Empty
        case expanded @ Enum(variants) =>
          val newSet = variants.get(matchedVariant.name).map {
            case Struct(setMembers) =>
              solveProduct(patternMembers, setMembers).map(Struct)
          }.orEmpty
          newSet.foldLeft(expanded - matchedVariant.name)(_ + _)
      }

    case ast.Pattern.Or(Typed(left, _), Typed(right, _), _) =>
      this - left - right
  }

  def +(s: MatchSet): MatchSet = (this, s) match {
    case (Sum(a), Sum(b)) => Sum(a ++ b)
    case (Sum(a), b) => Sum(a + b)
    case (a, Sum(b)) => Sum(b + a)
    case (Empty, a) => a
    case (a, Empty) => a
    case (Any(_), _) => this
    case (_, Any(_)) => this
    case (Enum(l), Enum(r))
      if (l.keySet intersect r.keySet).isEmpty =>

      Enum(l ++ r)
    case (a, b) => Sum(Set(a, b))
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

}

object MatchSet {

  def apply(p: Typed[ast.Pattern]): MatchSet = p match {
    case Typed(ast.Pattern.Var(_, _) | ast.Pattern.Ignore(_), t) => Any(t)
    case Typed(ast.Pattern.BoolLit(v, _), _) => Bool(v)
    case Typed(ast.Pattern.Struct(_, members, _, _), Type.Struct(_, allMembers)) =>
      val explicitMembers = members.map(_._1).toSet
      Struct(Nested(members).map(MatchSet.apply).value ++ allMembers.filterNot(m => explicitMembers(m.name)).map(m => (m.name, Any(m.typ))))
    case Typed(p, _) if p.isInfinitesimal => Empty
  }

  def solveProduct(patternMembers: List[(String, Typed[ast.Pattern])],
                   setMembers: List[(String, MatchSet)]): List[List[(String, MatchSet)]] = patternMembers match {

    case (memberName, pat) :: patrest =>
      val mismatched = setMembers.find(_._1 == memberName).get._2 - pat.expr
      val setMembersRest = setMembers.filter(_._1 != memberName)
      val mismatchedSet = mismatched match {
        case Empty => None
        case x => Some((memberName -> mismatched) :: setMembersRest)
      }

      val matched = MatchSet(pat)
      val matchedSets = solveProduct(patrest, setMembersRest)
        .map((memberName -> matched) :: _)
      mismatchedSet ++: matchedSets
    case Nil => Nil
  }

  final case class Any(typ: Type) extends MatchSet
  final case class Sum(sets: Set[MatchSet]) extends MatchSet {

    override def -(p: ast.Pattern): MatchSet = {
      val mapped = sets.map(_ - p).flatMap {
        case Empty => None
        case x => Some(x)
      }
      if(mapped.size == 1) mapped.head else Sum(mapped)
    }

  }
  final case object Empty extends MatchSet

  final case class Bool(member: Boolean) extends MatchSet

  final case class Struct(members: List[(String, MatchSet)]) extends MatchSet

  final case class Enum(variants: Map[String, Struct]) extends MatchSet {

    def -(variantName: String): MatchSet = {
      val newMap = variants - variantName
      if(newMap.isEmpty) Empty else Enum(newMap)
    }

  }

}
