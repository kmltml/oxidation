package oxidation
package parse

import fastparse.noApi._
import fastparse.WhitespaceApi
import oxidation.ast._
import oxidation.FunctorOps

/**
  * Created by Kamil on 15.02.2017.
  */
//noinspection ForwardReference
class Parser {

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper {
    import fastparse.all._
    CharsWhile(_.isWhitespace, min = 0)
  }
  import White._

  implicit val PFunctor: Functor[P] = new Functor[P] {
    override def map[A, B](a: P[A], f: A => B): P[B] = parserApi(a).map(f)
  }

  private val expression1: P[Expression] = P(
    intLiteral
  )
  private val expression3: P[Expression] = infixl(expression1, op3)
  private val expression4: P[Expression] = infixl(expression3, op4)
  private val expression5: P[Expression] = infixl(expression4, op5)
  private val expression6: P[Expression] = infixl(expression5, op6)
  private val expression7: P[Expression] = infixl(expression6, op7)

  val expression: P[Expression] = P(expression7)

  private def infixl(exp: => P[Expression], op: => P[InfixOp]): P[Expression] = P(
    exp ~ (op ~ exp).rep
  ).map { case (head, bs) =>
    bs.foldLeft(head) { case (a, (op, b)) => InfixApply(op, a, b) }
  }

  private val intLiteral: P[IntLit] =
    P(hexInt | decimalInt).map(IntLit)
  private val decimalInt: P[Int] =
    CharsWhile(_.isDigit).!.map(_.toInt)
  private val hexInt: P[Int] =
    ("0x" ~/ CharsWhile(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')).!)
      .map(Integer.parseInt(_, 16))

  private val op3: P[InfixOp] = P(
    LiteralStr("*").mapTo(InfixOp.Mul)
  | LiteralStr("/").mapTo(InfixOp.Div)
  | LiteralStr("%").mapTo(InfixOp.Mod)
  )
  private val op4: P[InfixOp] = P(
    LiteralStr("+").mapTo(InfixOp.Add)
  | LiteralStr("-").mapTo(InfixOp.Sub)
  )
  private val op5: P[InfixOp] = P(
    LiteralStr(">>").mapTo(InfixOp.Shr)
  | LiteralStr("<<").mapTo(InfixOp.Shl)
  )
  private val op6: P[InfixOp] = P(
    LiteralStr(">") .mapTo(InfixOp.Gt)
  | LiteralStr(">=").mapTo(InfixOp.Geq)
  | LiteralStr("<") .mapTo(InfixOp.Lt)
  | LiteralStr("<=").mapTo(InfixOp.Leq)
  )
  private val op7: P[InfixOp] = P(
    LiteralStr("==").mapTo(InfixOp.Eq)
  | LiteralStr("!=").mapTo(InfixOp.Neq)
  )

}
