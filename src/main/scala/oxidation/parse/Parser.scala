package oxidation
package parse

import fastparse.noApi._
import fastparse.WhitespaceApi
import sourcecode.Name
import oxidation.parse.ast._

import cats._
import cats.data._
import cats.implicits._

//noinspection ForwardReference
class Parser {

  val WS: P0 = {
    import fastparse.all._
    CharsWhile(_.isWhitespace, min = 0)
  }
  val WSNoNL: P0 = {
    import fastparse.all._
    CharsWhile(c => c.isWhitespace && !"\r\n".contains(c), min = 0) ~ !CharPred("\r\n".contains(_))
  }

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper(WS)
  import White._

  implicit val PFunctor: Functor[P] = new Functor[P] {
    def map[A, B](a: P[A])(f: A => B): P[B] = parserApi(a).map(f)
  }

  def whole[A](p: P[A]): P[A] = p ~ End

  val compilationUnit: P[Seq[TLD]] = P((WS ~~ tld).repX(sep = semi) ~~ End)

  private val expression0: P[Expression] = P(
    stringLiteral | intLiteral | varacc | parexp | blockexpr | ifexp | whileexp | boolLiteral
  )
  private val expression1: P[Expression] = P(
    expression0 ~~ postfix.repX
  ).map {
    case (exp, postfixes) => postfixes.foldLeft(exp)((e, f) => f(e))
  }
  private val expression2: P[Expression] = P(
    prefixOp.? ~ expression1
  ).map {
    case (Some(op), expr) => PrefixAp(op, expr)
    case (None, expr) => expr
  }
  private val expression3: P[Expression] = infixl(expression2, op3)
  private val expression4: P[Expression] = infixl(expression3, op4)
  private val expression5: P[Expression] = infixl(expression4, op5)
  private val expression6: P[Expression] = infixl(expression5, op6)
  private val expression7: P[Expression] = infixl(expression6, op7)
  private val expression8: P[Expression] =
    P(expression7 ~ (assignOp ~ expression).?).map {
      case (e, None) => e
      case (lval, Some((op, rval))) => Assign(lval, op, rval)
    }

  val expression: P[Expression] = P(expression8)

  val definition: P[Def] = P(
    defdef | valdef | vardef | structdef | enumdef | typedef
  )

  val tld: P[TLD] =
    P(definition | module | imprt)

  val module: P[Module] =
    P(K("module") ~ id.!.rep(sep = ".")).map(Module)

  val imprt: P[Import] = P {
    val path = id.!.rep(sep = ".", min = 1)
    val membersSelector = ("." ~ "{" ~/ id.!.rep(sep = ",", min = 1) ~ "}").map(ImportSpecifier.Members)
    val selector = membersSelector
    (K("import") ~ path ~ selector.?).map {
      case (path, Some(s)) => Import(path, s)
      case (path :+ "_", None) => Import(path, ImportSpecifier.All)
      case (path :+ last, None) => Import(path, ImportSpecifier.Members(Seq(last)))
    }
  }

  type Postfix = Expression => Expression
  private val postfix: P[Postfix] =
    P(apply | select)
  private val apply: P[Expression => App] =
    P(WSNoNL ~~ "(" ~/ expression.rep(sep = ",") ~ ")").map(params => App(_, params))
  private val select: P[Expression => Select] =
    P(WS ~~ "." ~/ id.!).map(id => Select(_, id))

  private val semi: P0 =
    P((";" | "\n" | "\r" | "\r\n") ~~ WS)

  private def infixl(exp: => P[Expression], op: => P[InfixOp]): P[Expression] = P(
    exp ~ (op ~/ exp).rep
  ).map { case (head, bs) =>
    bs.foldLeft(head) { case (a, (op, b)) => InfixAp(op, a, b) }
  }

  private def K(p: String): P0 = p ~~ !CharPred(c => c.isLetterOrDigit || idSpecialChars.contains(c))
  private def O(p: String): P0 = p ~~ !CharIn("~!%^&*+=<>|/?")

  val keyword: P0 =
    P(Seq("if", "else", "def", "while", "struct", "enum",
      "val", "var", "true", "false", "module", "import", "type").map(K).reduce(_ | _))

  private val idSpecialChars = "$_"
  private val idStart = CharPred(c => c.isLetter || idSpecialChars.contains(c))
  private val id: P0 = {
    val idRest  = CharsWhile(c => c.isLetterOrDigit || idSpecialChars.contains(c), min = 0)
    !keyword ~ idStart ~~ idRest
  }

  private val sym: P[Symbol] = P(id.!).map(Symbol.Unresolved)

  val typ: P[Type] = P(
    (namedType ~ typeApply.rep).map {
      case (t, paramLists) =>
        paramLists.foldLeft(t: Type)(Type.App(_, _))
    }
  )(Name("type"))

  private val namedType: P[Type.Named] = sym.map(Type.Named)

  private val typeApply: P[Seq[Type]] =
    P("[" ~/ typ.rep(sep = ",", min = 1) ~ "]")

  private val defdef: P[DefDef] = {
    val param = (id.! ~ ":" ~ typ).map(Param.tupled)
    val paramList = "(" ~ param.rep(sep = ",") ~ ")"

    P(K("def") ~/ sym ~ paramList.? ~ (":" ~/ typ).? ~ "=" ~ expression).map(DefDef.tupled)
  }

  private val defBody: P[(Symbol, Option[Type], Expression)] =
    sym ~ (":" ~ typ).? ~ O("=") ~/ expression

  private val valdef: P[ValDef] =
    P(K("val") ~/ defBody).map(ValDef.tupled)
  private val vardef: P[VarDef] =
    P(K("var") ~/ defBody).map(VarDef.tupled)

  private val structMember = (WS ~~ id.! ~~ WSNoNL ~~ ":" ~ typ).map(StructMember.tupled)

  private val structdef: P[StructDef] = {
    val body = "{" ~~ structMember.repX(sep = semi) ~ "}"
    val typeParams = "[" ~/ id.!.rep(sep = ",") ~ "]"
    P(K("struct") ~/ sym ~ typeParams.? ~ O("=") ~ body).map(StructDef.tupled)
  }

  private val enumdef: P[EnumDef] = {
    val typeParams = "[" ~/ id.!.rep(sep = ",") ~ "]"
    val variantMembers = "{" ~/ structMember.repX(sep = semi) ~ "}"
    val variant: P[EnumVariant] = (WS ~~ id.! ~ variantMembers.?).map {
      case (n, m) => EnumVariant(n, m getOrElse Seq.empty)
    }
    val body = "{" ~~ variant.repX(sep = semi) ~ "}"
    P(K("enum") ~/ sym ~ typeParams.? ~ O("=") ~/ body).map(EnumDef.tupled)
  }

  private val typedef: P[TypeAliasDef] = {
    val params = "[" ~/ id.!.rep(sep = ",") ~ "]"
    P(K("type") ~ sym ~ params.? ~ "=" ~/ typ).map(TypeAliasDef.tupled)
  }

  private val intLiteral: P[IntLit] =
    P(hexInt | decimalInt).map(IntLit)
  private val decimalInt: P[Int] =
    CharsWhile(_.isDigit).!.map(_.toInt)
  private val hexInt: P[Int] =
    ("0x" ~/ CharsWhile(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')).!)
      .map(Integer.parseInt(_, 16))

  private val boolLiteral: P[BoolLit] = P(
    K("true").as(BoolLit(true))
  | K("false").as(BoolLit(false))
  )

  private val stringLiteral: P[StringLit] = {
    val escapeSequence =
    ( LiteralStr("\\\"").as("\"")
    | LiteralStr("\\\\").as("\\")
    | LiteralStr("\\n").as("\n")
    )
    val stringChars = CharsWhile(!"\\\"".contains(_))
    P("\"".~/ ~~ (stringChars.! | escapeSequence).repX ~~ "\"").map(strs => StringLit(strs.mkString))
  }

  private val varacc: P[Var] =
    P(sym).map(Var)

  private val parexp: P[Expression] =
    P("(" ~/ expression ~ ")")

  private val blockexpr: P[Block] =
    P("{" ~/ (expression | definition).repX(sep = semi) ~ "}").map(Block)

  private val ifexp: P[If] = {
    val els = P(K("else") ~/ expression)
    P(K("if") ~/ "(" ~ expression ~ ")" ~/ expression ~ els.?).map(If.tupled)
  }

  private val whileexp: P[While] =
    P(K("while") ~/ "(" ~ expression ~ ")" ~/ expression).map(While.tupled)

  private val prefixOp: P[PrefixOp] = P(
    O("-").as(PrefixOp.Neg)
  | O("!").as(PrefixOp.Not)
  | O("~").as(PrefixOp.Inv)
  )

  private val assignOp: P[Option[InfixOp]] = P(
    O("=").as(None)
  | O("+=").as(Some(InfixOp.Add))
  | O("-=").as(Some(InfixOp.Sub))
  | O("*=").as(Some(InfixOp.Mul))
  | O("/=").as(Some(InfixOp.Div))
  | O("%=").as(Some(InfixOp.Mod))
  | O("<<=").as(Some(InfixOp.Shl))
  | O(">>=").as(Some(InfixOp.Shr))
  | O("^=").as(Some(InfixOp.Xor))
  | O("&=").as(Some(InfixOp.And))
  | O("|=").as(Some(InfixOp.Or))
  )

  private val op3: P[InfixOp] = P(
    O("*").as(InfixOp.Mul)
  | O("/").as(InfixOp.Div)
  | O("%").as(InfixOp.Mod)
  )
  private val op4: P[InfixOp] = P(
    O("+").as(InfixOp.Add)
  | O("-").as(InfixOp.Sub)
  )
  private val op5: P[InfixOp] = P(
    O(">>").as(InfixOp.Shr)
  | O("<<").as(InfixOp.Shl)
  )
  private val op6: P[InfixOp] = P(
    O(">=").as(InfixOp.Geq)
  | O(">") .as(InfixOp.Gt)
  | O("<=").as(InfixOp.Leq)
  | O("<") .as(InfixOp.Lt)
  )
  private val op7: P[InfixOp] = P(
    O("==").as(InfixOp.Eq)
  | O("!=").as(InfixOp.Neq)
  )

}
