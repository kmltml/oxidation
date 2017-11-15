package oxidation
package parse

import fastparse.noApi._
import fastparse.WhitespaceApi
import sourcecode.Name
import oxidation.parse.ast._
import cats._
import cats.data._
import cats.implicits._
import fastparse.core.Implicits.Sequencer

//noinspection ForwardReference
class Parser(file: Option[String]) {

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
    P(CharsWhile(_.isWhitespace) | NL | blockComment).rep
  }

  val White: WhitespaceApi.Wrapper = WhitespaceApi.Wrapper(WS)
  import White._

  implicit val PFunctor: Functor[P] = new Functor[P] {
    def map[A, B](a: P[A])(f: A => B): P[B] = parserApi(a).map(f)
  }

  def whole[A](p: P[A]): P[A] = p ~ End

  def located[A, R](p: P[A])(implicit seq: Sequencer[A, Span, R]): P[R] =
    (Index ~~ p ~~ Index) map { case (s, a, e) => seq(a, Span(file, s, e)) }

  val compilationUnit: P[Vector[TLD]] =
    P((WS ~~ tld).repX(sep = semi).map(_.toVector) ~ End)

  private val expression0: P[Expression] = P(
    stringLiteral | floatLiteral | intLiteral | structLit | charLiteral | unitLiteral |
    varacc | parexp | blockexpr | ifexp | whileexp | matchexp | boolLiteral
  )
  private val expression1: P[Expression] = P(
    expression0 ~~ postfix.repX
  ).map {
    case (exp, postfixes) => postfixes.foldLeft(exp)((e, f) => f(e))
  }
  private val expression2: P[Expression] = P(
    located(prefixOp.? ~ expression1)
  ).map {
    case (Some(op), expr, loc) => PrefixAp(op, expr, loc)
    case (None, expr, _) => expr
  }
  private val expression3: P[Expression] = infixexprl(expression2, op3)
  private val expression4: P[Expression] = infixexprl(expression3, op4)
  private val expression5: P[Expression] = infixexprl(expression4, op5)
  private val expression6: P[Expression] = infixexprl(expression5, op6)
  private val expression7: P[Expression] = infixexprl(expression6, op7)
  private val expression8: P[Expression] = infixexprl(expression7, op8)
  private val expression9: P[Expression] = infixexprl(expression8, op9)
  private val expression10: P[Expression] = infixexprl(expression9, op10)
  private val expression11: P[Expression] = infixexprl(expression10, op11)
  private val expression12: P[Expression] = infixexprl(expression11, op12)
  private val expression13: P[Expression] =
    P(located(expression12 ~ (assignOp ~ expression).?)).map {
      case (e, None, _) => e
      case (lval, Some((op, rval)), loc) => Assign(lval, op, rval, loc)
    }

  val expression: P[Expression] = P(expression13)

  val pattern0: P[Pattern] =
    P( located(K("_")).map(Pattern.Ignore)
     | parenPattern
     | pinnedPattern
     | aliasPattern
     | structPattern
     | varPattern
     | floatLiteral.map(_.pattern)
     | boolLiteral.map(_.pattern)
     | charLiteral.map(_.pattern)
     | intLiteral.map(_.pattern))

  val pattern1: P[Pattern] = infixl(pattern0, O("|")){ (_, a, b) => Pattern.Or(a, b, Span(file, a.loc.start, b.loc.end)) }

  val pattern: P[Pattern] = pattern1

  val structPattern: P[Pattern.Struct] = {
    val member = P(located(!K("_") ~ id.!) ~ ("=" ~/ pattern).?)
      .map { case (n, loc, p) => n -> p.getOrElse(Pattern.Var(Symbol.Unresolved(List(n)), loc)) }
    P(located(path.? ~ "{" ~/ member.repX(min = 1, sep = semiOrComa) ~ (semiOrComa ~ "_").!.? ~ "}"))
      .map { case (typ, ms, e, loc) => Pattern.Struct(typ.map(Symbol.Unresolved), ms.toList, e.nonEmpty, loc) }
  }

  val parenPattern: P[Pattern] =
    P(located("(" ~/ pattern ~ ")"))
      .map { case (p, s) => p.withLoc(s) }

  val aliasPattern: P[Pattern.Alias] =
    P(located(sym ~ O("@") ~ pattern))
      .map(Pattern.Alias.tupled)

  val pinnedPattern: P[Pattern.Pin] =
    P(located(O("^") ~ varacc))
      .map(Pattern.Pin.tupled)

  val varPattern: P[Pattern.Var] =
    P(located(path))
      .map { case (p, loc) => Pattern.Var(Symbol.Unresolved(p), loc) }

  val definition: P[Def] = P(
    defdef | valdef | vardef | structdef | enumdef | typedef
  )

  val tld: P[TLD] =
    P(definition | module | imprt)

  val module: P[Module] =
    P(K("module") ~ id.!.rep(sep = ".").map(_.toList)).map(Module)

  val path: P[List[String]] =
    P(id.!.rep(sep = ".", min = 1).map(_.toList))

  val imprt: P[Import] = P {
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
    P(typeApply | apply | method | select)
  private val apply: P[Expression => App] =
    P(WSNoNL ~~ "(" ~/ expression.rep(sep = ",").map(_.toList) ~ ")" ~~ Index).map { case (params, end) => prefix => App(prefix, params, prefix.loc.copy(end = end)) }
  private val typeApply: P[Expression => TypeApp] =
    P(WSNoNL ~~ typeParams ~~ Index).map { case (params, end) => prefix => TypeApp(prefix, params, prefix.loc.copy(end = end)) }
  private val select: P[Expression => Select] =
    P(WS ~~ "." ~/ id.! ~~ Index).map { case (id, end) => prefix => Select(prefix, id, prefix.loc.copy(end = end)) }
  private val method: P[Expression => Method] =
    P(WS ~~ ".." ~/ located(sym).rep(min = 1, sep = ".") ~~ Index).map {
      case ((head, headLoc) +: tail, end) =>
        val meth = tail.foldLeft(Var(head, headLoc): Expression) {
          case (l, (sym, loc)) => Select(l, sym.name, l.loc.copy(end = loc.end))
        }
        prefix => Method(prefix, meth, prefix.loc.copy(end = end))
    }

  private val semi: P0 =
    P(WSNoNL ~~ (";" | NL) ~~ WS)

  private val semiOrComa: P0 =
    P((";" | "," | NL) ~~ WS)

  private def infixl[Term, Op](exp: => P[Term], op: => P[Op])(ap: (Op, Term, Term) => Term): P[Term] = P(
    exp ~ (op ~/ exp).rep
  ).map { case (head, bs) =>
    bs.foldLeft(head) { case (a, (op, b)) => ap(op, a, b) }
  }

  private def infixr[Term, Op](exp: => P[Term], op: => P[Op])(ap: (Op, Term, Term) => Term): P[Term] = P(
    (NoCut(exp) ~ op).rep(min = 0) ~ exp
  ).map { case (as, last) =>
      as.foldRight(last) { case ((a, op), b) => ap(op, a, b) }
  }

  private def infixexprl(exp: => P[Expression], op: => P[InfixOp]): P[Expression] =
    infixl(exp, op){ (o, a, b) => InfixAp(o, a, b, Span(file, a.loc.start, b.loc.end)) }

  private def K(p: P0): P0 = p ~~ !CharPred(c => c.isLetterOrDigit || idSpecialChars.contains(c))
  private def O(p: String): P0 = p ~~ !CharIn("~!%^&*+=<>|/?")

  val keyword: P0 =
    P(K(StringIn("if", "else", "def", "while", "struct", "enum", "extern",
      "val", "var", "true", "false", "module", "import", "type", "match", "case")))

  private val idSpecialChars = "$_"
  private val idStart = P(CharPred(c => c.isLetter || idSpecialChars.contains(c)))
  private val id: P0 = {
    val idRest  = CharsWhile(c => c.isLetterOrDigit || idSpecialChars.contains(c), min = 0)
    !keyword ~ idStart ~~ idRest
  }

  private val sym: P[Symbol] = P(id.!).map(n => Symbol.Unresolved(List(n)))

  val typ0: P[TypeName] = P(
    (namedType ~ typeParams.rep).map {
      case (t, paramLists) =>
        paramLists.foldLeft(t: TypeName)(TypeName.App(_, _))
    } | intType
  )(Name("type"))

  val typ1: P[TypeName] = {
    val params: P[List[TypeName]] = P(("(" ~/ typ.rep(sep = ",".~/).map(_.toList) ~ ")") | NoCut(typ0).map(List(_)))
    P((params ~ O("=>")).rep(min = 0) ~ typ0)
      .map { case (as, last) => as.foldRight(last)((a, b) => TypeName.Fun(a, b)) }
  }

  val typ: P[TypeName] = typ1

  private val namedType: P[TypeName.Named] = sym.map(TypeName.Named)

  private val intType: P[TypeName.IntLiteral] =
    P(decimalInt | hexInt).map(TypeName.IntLiteral)

  private val typeParams: P[List[TypeName]] =
    P("[" ~/ typ.rep(sep = ",", min = 1).map(_.toList) ~ "]")

  private val defdef: P[DefDef] = {
    val param = (id.! ~ ":" ~ typ).map(Param.tupled)
    val paramList = "(" ~ param.rep(sep = ",").map(_.toList) ~ ")"

    val extern = located(K("extern")).map(Extern)

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
    val body = "{" ~~ structMember.repX(sep = semiOrComa).map(_.toList) ~ "}"
    val typeParams = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    P(K("struct") ~/ sym ~ typeParams.? ~ O("=") ~ body).map(StructDef.tupled)
  }

  private val enumdef: P[EnumDef] = {
    val typeParams = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    val variantMembers = "{" ~/ structMember.repX(sep = semiOrComa).map(_.toList) ~ "}"
    val variant: P[EnumVariantDef] = (WS ~~ sym ~ variantMembers.?).map {
      case (n, m) => EnumVariantDef(n, m getOrElse Nil)
    }
    val body = "{" ~~ variant.repX(sep = semi).map(_.toList) ~ "}"
    P(K("enum") ~/ sym ~ typeParams.? ~ O("=") ~/ body).map(EnumDef.tupled)
  }

  private val typedef: P[TypeAliasDef] = {
    val params = "[" ~/ id.!.rep(sep = ",").map(_.toList) ~ "]"
    P(K("type") ~ sym ~ params.? ~ "=" ~/ typ).map(TypeAliasDef.tupled)
  }

  private val digits: P0 = CharsWhile(_.isDigit)

  private val intLiteral: P[IntLit] =
    P(located(hexInt | decimalInt)).map(IntLit.tupled)
  private val decimalInt: P[Long] =
    digits.!.map(java.lang.Long.parseUnsignedLong)
  private val hexInt: P[Long] =
    ("0x" ~/ CharsWhile(c => c.isDigit || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')).!)
      .map(java.lang.Long.parseUnsignedLong(_, 16))

  private val floatLiteral: P[FloatLit] =
    P(located(
     ( digits ~~ "." ~~ digits ~~ exponent.?
     | digits ~~ exponent).!)).map { case (s, loc) => FloatLit(BigDecimal(s), loc) }
  private val exponent: P0 =
    P(CharIn("eE") ~~ CharIn("+-").? ~~ digits)


  private val boolLiteral: P[BoolLit] = P(
    located(K("true")).map(BoolLit(true, _))
  | located(K("false")).map(BoolLit(false, _))
  )

  private val charLiteral: P[CharLit] = P(
    located("'" ~~
    ( LiteralStr("\\\\").as('\\')
    | LiteralStr("\\'").as('\'')
    | LiteralStr("\\n").as('\n')
    | LiteralStr("\\0").as('\0')
    | CharPred(_ != '\'').!.map(_.head)
    ) ~~ "'")
  ).map(CharLit.tupled)

  private val unitLiteral: P[UnitLit] =
    P(located("()")).map(UnitLit)

  private val stringLiteral: P[StringLit] = {
    val escapeSequence =
    ( LiteralStr("\\\"").as("\"")
    | LiteralStr("\\\\").as("\\")
    | LiteralStr("\\n").as("\n")
    | LiteralStr("\\0").as("\0")
    )
    val stringChars = CharsWhile(!"\\\"".contains(_))
    P(located("\"".~/ ~~ (stringChars.! | escapeSequence).repX ~~ "\"")).map { case (strs, loc) => StringLit(strs.mkString, loc) }
  }

  private val structLit: P[StructLit] = {
    val member = P(id.! ~ "=" ~ expression)
    P(located(path ~ NoCut(typeParams).? ~ "{" ~/ member.repX(sep = semiOrComa).map(_.toList) ~ "}"))
      .map { case (typ, params, ms, loc) => StructLit(Symbol.Unresolved(typ), params, ms, loc) }
  }

  private val varacc: P[Var] =
    P(located(sym)).map(Var.tupled)

  private val parexp: P[Expression] =
    P(located("(" ~/ expression ~ ")")).map { case (e, loc) => e.withLoc(loc) }

  private val blockexpr: P[Block] =
    P(located("{" ~/ (expression | definition).repX(sep = semi) ~ "}")).map { case (s, loc) => Block(s.toVector, loc) }

  private val ifexp: P[If] = {
    val els = P(K("else") ~/ expression)
    P(located(K("if") ~/ "(" ~ expression ~ ")" ~/ expression ~ els.?)).map(If.tupled)
  }

  private val whileexp: P[While] =
    P(located(K("while") ~/ "(" ~ expression ~ ")" ~/ expression)).map(While.tupled)

  private val matchexp: P[Match] = {
    val guard = K("if") ~/ expression
    val cas = (K("case") ~/ pattern ~ guard.? ~ O("=>") ~ expression)
      .map(MatchCase.tupled)
    P(located(K("match") ~/ "(" ~ expression ~ ")" ~/ "{" ~ cas.rep(min = 1) ~ "}"))
      .map { case (m, cs, l) => Match(m, cs.toList, l) }
  }

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
