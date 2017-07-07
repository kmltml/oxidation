package oxidation
package analyze

sealed trait MatchSet {

  import MatchSet._

  def -(p: ast.Pattern): MatchSet = p match {
    case ast.Pattern.Ignore(_) | ast.Pattern.Var(_, _) => Empty
    case ast.Pattern.IntLit(_, _) | ast.Pattern.FloatLit(_, _) | ast.Pattern.CharLit(_, _) => this
    case ast.Pattern.BoolLit(patbool, _) => this match {
      case Any(Type.U1) => Bool(!patbool)
      case Bool(`patbool`) => Empty
      case _ => this
    }
  }

  def isEmpty: Boolean = this match {
    case Empty => true
    case _ => false
  }

}

object MatchSet {

  final case class Any(typ: Type) extends MatchSet
  final case object Empty extends MatchSet

  final case class Bool(member: Boolean) extends MatchSet

}
