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

  val blockComment: P0 = {
    import fastparse.all._
    P("/*" ~/ (CharsWhile(c => c != '/' && c != '*') | ("*" ~ !"/") | ("/" ~ !"*") | blockComment).rep ~ "*/")
  }

  val WSNoNL: P0 = {
    import fastparse.all._
    P((CharsWhile(c => c.isWhitespace && !"\r\n".contains(c), min = 1) | NoCut(blockComment)).rep(min = 0))
  }
  val NL: P0 = {
    import fastparse.all._
    val lineComment = "//" ~/ CharsWhile(!"\r\n".contains(_), min = 0)
    P("\r\n" | "\r" | "\n" | lineComment)
  }
  val WS: P0 = {
    import fastparse.all._
    (CharsWhile(_.isWhitespace) | NL | blockComment).rep
  }

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper(WS)
  import White._

  implicit val PFunctor: Functor[P] = new Functor[P] {
    def map[A, B](a: P[A])(f: A => B): P[B] = parserApi(a).map(f)
  }

  def whole[A](p: P[A]): P[A] = p ~ End

  val compilationUnit: P[Vector[TLD]] =
    P((WS ~~ tld).repX(sep = semi).map(_.toVector) ~~ End)

  private val expression0: P[Expression] = P(
    stringLiteral | intLiteral | structLit | charLiteral | unitLiteral | varacc | parexp | blockexpr | ifexp | whileexp | boolLiteral
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
  private val expression8: P[Expression] = infixl(expression7, op8)
  private val expression9: P[Expression] = infixl(expression8, op9)
  private val expression10: P[Expression] = infixl(expression9, op10)
  private val expression11: P[Expression] = infixl(expression10, op11)
  private val expression12: P[Expression] = infixl(expression11, op12)
  private val expression13: P[Expression] =
    P(expression12 ~ (assignOp ~ expression).?).map {
      case (e, None) => e
      case (lval, Some((op, rval))) => Assign(lval, op, rval)
    }

  val expression: P[Expression] = P(expression13)

  val definition: P[Def] = P(
    defdef | valdef | vardef | structdef | enumdef | typedef
  )

  val tld: P[TLD] =
    P(definition | module | imprt)

  val module: P[Module] =
    P(K("module") ~ id.!.rep(sep = ".").map(_.toList)).map(Module)

  val imprt: P[Import] = P {
    val path = id.!.rep(sep = ".", min = 1).map(_.toList)
    val membersSelector = ("." ~ "{" ~/ id.!.rep(sep = ",", min = 1).map(_.toList) ~ "}").map(ImportSpecifier.Members)
    val selector = membersSelector
    (K("import") ~ path ~ selector.?).map {
      case (path, Some(s)) => Import(path, s)
      case (path :+ "_", None) => Import(path, ImportSpecifier.All)
      case (path :+ last, None) => Import(path, ImportSpecifier.Members(List(last)))
    }
  }

  type Postfix = Expression => Expression
  private val postfix: P[Postfix] =
    P(typeApply | apply | select)
  private val apply: P[Expression => App] =
    P(WSNoNL ~~ "(" ~/ expression.rep(sep = ",").map(_.toList) ~ ")").map(params => App(_, params))
  private val typeApply: P[Expression => TypeApp] =
    P(WSNoNL ~~ typeParams).map(params => TypeApp(_, params))
  private val select: P[Expression => Select] =
    P(WS ~~ "." ~/ id.!).map(id => Select(_, id))

  private val semi: P0 =
    P(WSNoNL ~~ (";" | NL) ~~ WS)

  private val semiOrComa: P0 =
    P((";" | "," | NL) ~~ WS)

  private def infixl(exp: => P[Expression], op: => P[InfixOp]): P[Expression] = P(
    exp ~ (op ~/ exp).rep
  ).map { case (head, bs) =>
    bs.foldLeft(head) { case (a, (op, b)) => InfixAp(op, a, b) }
  }

  private def K(p: P0): P0 = p ~~ !CharPred(c => c.isLetterOrDigit || idSpecialChars.contains(c))
  private def O(p: String): P0 = p ~~ !CharIn("~!%^&*+=<>|/?")

  val keyword: P0 =
    P(K(StringIn("if", "else", "def", "while", "struct", "enum", "extern",
      "val", "var", "true", "false", "module", "import", "type")))

  private val idSpecialChars = "$_"
  private val idStart = CharPred(c => c.isLetter || idSpecialChars.contains(c))
  private val id: P0 = {
    val idRest  = CharsWhile(c => c.isLetterOrDigit || idSpecialChars.contains(c), min = 0)
    !keyword ~ idStart ~~ idRest
  }

  private val sym: P[Symbol] = P(id.!).map(Symbol.Unresolved)

  val typ: P[TypeName] = P(
    (namedType ~ typeParams.rep).map {
      case (t, paramLists) =>
        paramLists.foldLeft(t: TypeName)(TypeName.App(_, _))
    } | intType
  )(Name("type"))

  private val namedType: P[TypeName.Named] = sym.map(TypeName.Named)

  private val intType: P[TypeName.IntLiteral] =
    P(decimalInt | hexInt).map(TypeName.IntLiteral)

  private val typeParams: P[List[TypeName]] =
    P("[" ~/ typ.rep(sep = ",", min = 1).map(_.toList) ~ "]")

  private val defdef: P[DefDef] = {
    val param = (id.! ~ ":" ~ typ).map(Param.tupled)
    val paramList = "(" ~ param.rep(sep = ",").map(_.toList) ~ ")"

    val extern = K("extern").as(Extern())

    P(K("def") ~/ sym ~ paramList.? ~ (":" ~/ typ).? ~ "=" ~ (expression | extern)).map(DefDef.tupled)
  }

  private val defBody: P[(Symbol, Option[TypeName], Expression)] =
    sym ~ (":" ~ typ).? ~ O("=") ~/ expression

  private val valdef: P[ValDef] =
    P(K("val") ~/ defBody).map(ValDef.tupled)
  private val vardef: P[VarDef] =
    P(K("var") ~/ defBody).map(VarDef.tupled)

  private val structMember = (WS ~~ id.! ~~ WSNoNL ~~ ":" ~ typ).map(StructMemberDef.tupled)

  private val structdef: P[StructDef] = {
    val body = "{" ~~ structMember.repX(sep = semi).map(_.toList) ~ "}"
    val typeParams = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    P(K("struct") ~/ sym ~ typeParams.? ~ O("=") ~ body).map(StructDef.tupled)
  }

  private val enumdef: P[EnumDef] = {
    val typeParams = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    val variantMembers = "{" ~/ structMember.repX(sep = semi).map(_.toList) ~ "}"
    val variant: P[EnumVariant] = (WS ~~ id.! ~ variantMembers.?).map {
      case (n, m) => EnumVariant(n, m getOrElse Nil)
    }
    val body = "{" ~~ variant.repX(sep = semi).map(_.toList) ~ "}"
    P(K("enum") ~/ sym ~ typeParams.? ~ O("=") ~/ body).map(EnumDef.tupled)
  }

  private val typedef: P[TypeAliasDef] = {
    val params = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    P(K("type") ~ sym ~ params.? ~ "=" ~/ typ).map(TypeAliasDef.tupled)
  }

  private val intLiteral: P[IntLit] =
    P(hexInt | decimalInt).map(IntLit)
  private val decimalInt: P[Long] =
    CharsWhile(_.isDigit).!.map(java.lang.Long.parseUnsignedLong)
  private val hexInt: P[Long] =
    ("0x" ~/ CharsWhile(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')).!)
      .map(java.lang.Long.parseUnsignedLong(_, 16))

  private val boolLiteral: P[BoolLit] = P(
    K("true").as(BoolLit(true))
  | K("false").as(BoolLit(false))
  )

  private val charLiteral: P[CharLit] = P(
    "'" ~~
    ( LiteralStr("\\\\").as('\\')
    | LiteralStr("\\'").as('\'')
    | LiteralStr("\\n").as('\n')
    | LiteralStr("\\0").as('\0')
    | CharPred(_ != '\'').!.map(_.head)
    ) ~~ "'"
  ).map(CharLit)

  private val unitLiteral: P[UnitLit] =
    P("()").as(UnitLit())

  private val stringLiteral: P[StringLit] = {
    val escapeSequence =
    ( LiteralStr("\\\"").as("\"")
    | LiteralStr("\\\\").as("\\")
    | LiteralStr("\\n").as("\n")
    )
    val stringChars = CharsWhile(!"\\\"".contains(_))
    P("\"".~/ ~~ (stringChars.! | escapeSequence).repX ~~ "\"").map(strs => StringLit(strs.mkString))
  }

  private val structLit: P[StructLit] = {
    val member = P(id.! ~ "=" ~ expression)
    P(sym ~ "{" ~/ member.repX(sep = semiOrComa).map(_.toList) ~ "}").map(StructLit.tupled)
  }

  private val varacc: P[Var] =
    P(sym).map(Var)

  private val parexp: P[Expression] =
    P("(" ~/ expression ~ ")")

  private val blockexpr: P[Block] =
    P("{" ~/ (expression | definition).repX(sep = semi) ~ "}").map(s => Block(s.toVector))

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
  | O("&=").as(Some(InfixOp.BitAnd))
  | O("|=").as(Some(InfixOp.BitOr))
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
  private val op8: P[InfixOp] = P(O("&") as InfixOp.BitAnd)
  private val op9: P[InfixOp] = P(O("^") as InfixOp.Xor)
  private val op10: P[InfixOp] = P(O("|") as InfixOp.BitOr)
  private val op11: P[InfixOp] = P(O("&&") as InfixOp.And)
  private val op12: P[InfixOp] = P(O("||") as InfixOp.Or)

}
