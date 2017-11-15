package oxidation
package parse

import ast._
import fastparse.core.Parsed
import utest._

object ParserTests extends TestSuite with MatchCaseSyntax {

  def assertFail(res: Parsed[_, _, _]): Unit = assertMatch(res) {
    case _: Parsed.Failure[_, _] =>
  }

  implicit def unresolvedSymbol(n: String): Symbol = Symbol.Unresolved(List(n))
  def u(ns: scala.Symbol*): Symbol = Symbol.Unresolved(ns.map(_.name).toList)

  implicit def span(s: (Int, Int)): Span = Span(None, s._1, s._2)

  implicit class SpanConstruct(private val x: Int) {

    def +>(l: Int): Span = x -> (x + l)

  }

  implicit class NewlineNormalizer(private val x: String) {
    def normalize: String = x.replaceAll("\\r\\n", "\n")
  }

  val tests = apply {
    val p = new Parser(None)
    "expression should parse: " - {
      val expr = p.whole(p.expression)
      "int literals" - {
        expr.parse("42").get.value ==> IntLit(42, 0 +> 2)
        expr.parse("0xdeaf").get.value ==> IntLit(0xdeaf, 0 +> 6)
        expr.parse("0xffffffffffffffff").get.value ==> IntLit(-1, 0 +> 18)
      }
      "FloatLit" - {
        expr.parse("0.1").get.value ==> FloatLit(BigDecimal("0.1"), 0 +> 3)
        expr.parse("1e+10").get.value ==> FloatLit(BigDecimal("1e+10"), 0 +> 5)
        expr.parse("1e10").get.value ==> FloatLit(BigDecimal("1e10"), 0 +> 4)
        expr.parse("1e-10").get.value ==> FloatLit(BigDecimal("1e-10"), 0 +> 5)
      }
      "bool literals" - {
        expr.parse("true").get.value ==> BoolLit(true, 0 +> 4)
        expr.parse("false").get.value ==> BoolLit(false, 0 +> 5)
      }
      "char literals" - {
        expr.parse("'a'").get.value ==> CharLit('a', 0 +> 3)
        expr.parse("'0'").get.value ==> CharLit('0', 0 +> 3)
        expr.parse("'\\n'").get.value ==> CharLit('\n', 0 +> 4)
        expr.parse("'\\\\'").get.value ==> CharLit('\\', 0 +> 4)
        expr.parse("'\\''").get.value ==> CharLit('\'', 0 +> 4)
        expr.parse("'\\0'").get.value ==> CharLit('\0', 0 +> 4)
      }
      "string literals" - {
        expr.parse(""" "Hello, \"world\"!\n" """).get.value ==> StringLit("Hello, \"world\"!\n", 1 +> 21)
        expr.parse(""" "Hello, obsolete, zero-terminated world!\0" """).get.value ==> StringLit("Hello, obsolete, zero-terminated world!\0", 1 +> 43)
      }
      "struct literals" - {
        expr.parse(
          """Vector {
            |  x = 10
            |  y = 20
            |}
          """.stripMargin.normalize).get.value ==> StructLit(u('Vector), None, List(
            "x" -> IntLit(10, 15 +> 2),
            "y" -> IntLit(20, 24 +> 2)
          ), 0 +> 28)
        expr.parse(
          "Vector { x = 10, y = 20 }").get.value ==> StructLit(u('Vector), None, List(
            "x" -> IntLit(10, 13 +> 2),
            "y" -> IntLit(20, 21 +> 2)
          ), 0 +> 25)
        "with path name" - {
          expr.parse("Option.Some { value = 10 }").get.value ==>
            StructLit(u('Option, 'Some), None, List(
              "value" -> IntLit(10, 22 +> 2)
            ), 0 +> 26)
        }
        "with type params" - {
          expr.parse("Pair[i32, u1] { _1 = 10, _2 = false }").get.value ==>
            StructLit(u('Pair),
              Some(List(TypeName.Named(u('i32)),
                TypeName.Named(u('u1)))),
              List(
                "_1" -> IntLit(10, 21 +> 2),
                "_2" -> BoolLit(false, 30 +> 5)
              ), 0 +> 37)
        }
      }
      "unit literal" - {
        expr.parse("()").get.value ==> UnitLit(0 +> 2)
      }
      "addition" - {
        expr.parse("2 + 3").get.value ==> InfixAp(InfixOp.Add, IntLit(2, 0 +> 1), IntLit(3, 4 +> 1), 0 +> 5)
        expr.parse("2 + 3 + 4").get.value ==>
          InfixAp(InfixOp.Add, InfixAp(InfixOp.Add, IntLit(2, 0 +> 1), IntLit(3, 4 +> 1), 0 +> 5), IntLit(4, 8 +> 1), 0 +> 9)
      }
      "multiplication" - {
        expr.parse("2 * 3").get.value ==> InfixAp(InfixOp.Mul, IntLit(2, 0 +> 1), IntLit(3, 4 +> 1), 0 +> 5)
      }
      "InfixAp" - {
        "And"    - { expr.parse("true && true").get.value ==> InfixAp(InfixOp.And, BoolLit(true, 0 +> 4), BoolLit(true, 8 +> 4), 0 +> 12) }
        "Or"     - { expr.parse("true || true").get.value ==> InfixAp(InfixOp.Or, BoolLit(true, 0 +> 4), BoolLit(true, 8 +> 4), 0 +> 12) }
        "BitOr"  - { expr.parse("true | true").get.value  ==> InfixAp(InfixOp.BitOr, BoolLit(true, 0 +> 4), BoolLit(true, 7 +> 4), 0 +> 11) }
        "BitAnd" - { expr.parse("true & true").get.value  ==> InfixAp(InfixOp.BitAnd, BoolLit(true, 0 +> 4), BoolLit(true, 7 +> 4), 0 +> 11) }
        "Xor"    - { expr.parse("true ^ true").get.value  ==> InfixAp(InfixOp.Xor, BoolLit(true, 0 +> 4), BoolLit(true, 7 +> 4), 0 +> 11) }
      }
      "mixed priorities" - {
        expr.parse("2 + 3 * 4").get.value ==>
          InfixAp(InfixOp.Add, IntLit(2, 0 +> 1), InfixAp(InfixOp.Mul, IntLit(3, 4 +> 1), IntLit(4, 8 +> 1), 4 +> 5), 0 +> 9)
      }
      "var access" - {
        for (name <- Seq("foo", "foo2", "Bar", "foo_bar", "$foobar", "_foo", "x", "if_", "definition")) {
          expr.parse(name).get.value ==> Var(name, 0 +> name.length)
        }
        assertFail(expr.parse("foo bar"))
        assertFail(expr.parse("if"))
      }
      "simple arithmetic involving vars" - {
        expr.parse("i + 1").get.value ==>
          InfixAp(InfixOp.Add, Var("i", 0 +> 1), IntLit(1, 4 +> 1), 0 +> 5)
        expr.parse("foo >= bar").get.value ==>
          InfixAp(InfixOp.Geq, Var("foo", 0 +> 3), Var("bar", 7 +> 3), 0 +> 10)
      }
      "parenthesised expression" - {
        expr.parse("(a + b) * c").get.value ==>
          InfixAp(InfixOp.Mul, InfixAp(InfixOp.Add, Var("a", 1 +> 1), Var("b", 5 +> 1), 0 +> 7), Var("c", 10 +> 1), 0 +> 11)
      }
      "a prefix operator application" - {
        expr.parse("-1").get.value ==> PrefixAp(PrefixOp.Neg, IntLit(1, 1 +> 1), 0 +> 2)
        expr.parse("~1").get.value ==> PrefixAp(PrefixOp.Inv, IntLit(1, 1 +> 1), 0 +> 2)
        expr.parse("!1").get.value ==> PrefixAp(PrefixOp.Not, IntLit(1, 1 +> 1), 0 +> 2)
        expr.parse("-1 + ~2").get.value ==>
          InfixAp(InfixOp.Add,
            PrefixAp(PrefixOp.Neg, IntLit(1, 1 +> 1), 0 +> 2),
            PrefixAp(PrefixOp.Inv, IntLit(2, 6 +> 1), 5 +> 2), 0 +> 7)
      }
      "a block expression" - {
        expr.parse("{ 1 }").get.value ==> Block(Vector(IntLit(1, 2 +> 1)), 0 +> 5)
        expr.parse(
          """{
            |  foo
            |  bar; baz
            |}
          """.stripMargin.normalize).get.value ==>
            Block(Vector(
              Var("foo", 4 +> 3), Var("bar", 10 +> 3), Var("baz", 15 +> 3)
            ), 0 +> 20)
        assertFail(expr.parse("""{ foo bar }"""))
        expr.parse(
          """{
            |  foo
            |  (bar)
            |}
          """.stripMargin.normalize).get.value ==>
            Block(Vector(
              Var("foo", 4 +> 3), Var("bar", 10 +> 5)
            ), 0 +> 17)
        expr.parse(
          """{
            |  val x = 10
            |  x + 1
            |}
          """.stripMargin.normalize).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10, 12 +> 2)),
              InfixAp(InfixOp.Add, Var("x", 17 +> 1), IntLit(1, 21 +> 1), 17 +> 5)
            ), 0 +> 24)
        expr.parse(
          """{
            |  val x = 10
            |  val y = 20
            |  x + y
            |}
          """.stripMargin.normalize).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10, 12 +> 2)),
              ValDef("y", None, IntLit(20, 25 +> 2)),
              InfixAp(InfixOp.Add, Var("x", 30 +> 1), Var("y", 34 +> 1), 30 +> 5)
            ), 0 +> 37)
        "whitespace before semicolon" - {
          expr.parse(
            """{
              |  val x = 10 // hello
              |  x + 20
              |}
            """.stripMargin.normalize).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10, 12 +> 2)),
              InfixAp(InfixOp.Add, Var("x", 26 +> 1), IntLit(20, 30 +> 2), 26 +> 6)
            ), 0 +> 34)
          expr.parse(
            "{\n  val x = 10    \n  x + 20\n}").get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10, 12 +> 2)),
              InfixAp(InfixOp.Add, Var("x", 21 +> 1), IntLit(20, 25 +> 2), 21 +> 6)
            ), 0 +> 29)
          expr.parse(
            """{
              |  val x = 10 /* hidere */
              |  x + 20
              |}
            """.stripMargin.normalize).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10, 12 +> 2)),
              InfixAp(InfixOp.Add, Var("x", 30 +> 1), IntLit(20, 34 +> 2), 30 +> 6)
            ), 0 +> 38)
        }
      }
      "MethodApp" - {
        "without params" - {
          expr.parse("obj..meth").get.value ==> Method(Var("obj", 0 +> 3), Var("meth", 5 +> 4), 0 +> 9)
        }
        "with params" - {
          expr.parse("obj..meth(arg)").get.value ==>
            App(Method(Var("obj", 0 +> 3), Var("meth", 5 +> 4), 0 +> 9), List(Var("arg", 10 +> 3)), 0 +> 14)
        }
        "path in method name" - {
          expr.parse("obj..a.b(arg)").get.value ==>
            App(Method(Var("obj", 0 +> 3), Select(Var("a", 5 +> 1), "b", 5 +> 3), 0 +> 8),
              List(Var("arg", 9 +> 3)), 0 +> 13)
        }
      }
      "a function call" - {
        expr.parse("foo()").get.value ==> App(Var("foo", 0 +> 3), Nil, 0 +> 5)
        expr.parse("foo(1, 2)").get.value ==> App(Var("foo", 0 +> 3), List(IntLit(1, 4 +> 1), IntLit(2, 7 +> 1)), 0 +> 9)
        expr.parse("foo(1)(2)").get.value ==> App(App(Var("foo", 0 +> 3), List(IntLit(1, 4 +> 1)), 0 +> 6), List(IntLit(2, 7 +> 1)), 0 +> 9)
      }
      "a template application" - {
        expr.parse("foo[i32]").get.value ==> TypeApp(Var("foo", 0 +> 3), List(TypeName.Named("i32")), 0 +> 8)
        expr.parse("identity[i32](42)").get.value ==>
          App(TypeApp(Var("identity", 0 +> 8), List(TypeName.Named("i32")), 0 +> 13), List(IntLit(42, 14 +> 2)), 0 +> 17)
      }
      "a member access" - {
        expr.parse("foo.bar").get.value ==> Select(Var("foo", 0 +> 3), "bar", 0 +> 7)
      }
      "an if expression" - {
        expr.parse("if(true) 1 else 0").get.value ==> If(BoolLit(true, 3 +> 4), IntLit(1, 9 +> 1), Some(IntLit(0, 16 +> 1)), 0 +> 17)
        expr.parse("if(foo) bar()").get.value ==> If(Var("foo", 3 +> 3), App(Var("bar", 8 +> 3), Nil, 8 +> 5), None, 0 +> 13)
      }
      "a while loop" - {
        expr.parse("while(foo) bar()").get.value ==> While(Var("foo", 6 +> 3), App(Var("bar", 11 +> 3), Nil, 11 +> 5), 0 +> 16)
      }
      "a match expression" - {
        "simple" - {
          expr.parse(
            """match(foo) {
              |  case 10 => 20
              |  case x => x + 2
              |}""".stripMargin.normalize).get.value ==> Match(Var("foo", 6 +> 3), List(
            Pattern.IntLit(10, 20 +> 2) -> IntLit(20, 26 +> 2),
            Pattern.Var("x", 36 +> 1) -> InfixAp(InfixOp.Add, Var("x", 41 +> 1), IntLit(2, 45 +> 1), 41 +> 5)
          ), 0 +> 48)
        }
        "with guards" - {
          expr.parse(
            """match(foo) {
              |  case x if x >= 10 => x
              |}
            """.stripMargin.normalize).get.value ==>
            Match(
              Var("foo", 6 +> 3), List(
                MatchCase(Pattern.Var("x", 20 +> 1), guard = Some(InfixAp(InfixOp.Geq, Var("x", 25 +> 1), IntLit(10, 30 +> 2), 25 +> 7)),
                  Var("x", 36 +> 1))
              ), 0 +> 39
            )
        }
      }
      "a variable assignment" - {
        expr.parse("foo = 42").get.value ==> Assign(Var("foo", 0 +> 3), None, IntLit(42, 6 +> 2), 0 +> 8)
        expr.parse("foo.bar(32).baz += 6").get.value ==>
          Assign(Select(App(Select(Var("foo", 0 +> 3), "bar", 0 +> 7), List(IntLit(32, 8 +> 2)), 0 +> 11), "baz", 0 +> 15), Some(InfixOp.Add), IntLit(6, 19 +> 1), 0 +> 20)

        expr.parse("foo = bar").get.value ==> Assign(Var("foo", 0 +> 3), None, Var("bar", 6 +> 3), 0 +> 9)
        expr.parse("foo += bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Add), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo -= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Sub), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo *= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Mul), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo /= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Div), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo %= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Mod), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo <<= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Shl), Var("bar", 8 +> 3), 0 +> 11)
        expr.parse("foo >>= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Shr), Var("bar", 8 +> 3), 0 +> 11)
        expr.parse("foo ^= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.Xor), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo &= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.BitAnd), Var("bar", 7 +> 3), 0 +> 10)
        expr.parse("foo |= bar").get.value ==> Assign(Var("foo", 0 +> 3), Some(InfixOp.BitOr), Var("bar", 7 +> 3), 0 +> 10)
      }
    }

    "Pattern" - {
      val pat = p.whole(p.pattern)
      "Var" - (pat.parse("xyz").get.value ==> Pattern.Var("xyz", 0 +> 3))
      "Var path" - (pat.parse("x.y.z").get.value ==> Pattern.Var(Symbol.Unresolved(List("x", "y", "z")), 0 +> 5))
      "Ignore" - (pat.parse("_").get.value ==> Pattern.Ignore(0 +> 1))
      "IntLit" - (pat.parse("10").get.value ==> Pattern.IntLit(10, 0 +> 2))
      "FloatLit" - (pat.parse("10.0").get.value ==> Pattern.FloatLit(10.0, 0 +> 4))
      "BoolLit" - {
        "true" - (pat.parse("true").get.value ==> Pattern.BoolLit(true, 0 +> 4))
        "false" - (pat.parse("false").get.value ==> Pattern.BoolLit(false, 0 +> 5))
      }
      "CharLit" - (pat.parse("'a'").get.value ==> Pattern.CharLit('a', 0 +> 3))
      "Struct" - {
        "basic explicit" - {
          pat.parse("point { x = 10, y = _ }").get.value ==>
            Pattern.Struct(Some("point"), List(
              "x" -> Pattern.IntLit(10, 12 +> 2),
              "y" -> Pattern.Ignore(20 +> 1)
            ), ignoreExtra = false, 0 +> 23)
        }
        "explicit path" -{
          pat.parse("option.some { value }").get.value ==>
          Pattern.Struct(Some(Symbol.Unresolved(List("option", "some"))), List(
            "value" -> Pattern.Var("value", 14 +> 5)
          ), ignoreExtra = false, 0 +> 21)
        }
        "basic implicit" - {
          pat.parse("{ x = 10, y = _ }").get.value ==>
            Pattern.Struct(None, List(
              "x" -> Pattern.IntLit(10, 6 +> 2),
              "y" -> Pattern.Ignore(14 +> 1)
            ), ignoreExtra = false, 0 +> 17)
        }
        "automatic name bind" - {
          pat.parse("{ x, y }").get.value ==>
            Pattern.Struct(None, List(
              "x" -> Pattern.Var("x", 2 +> 1),
              "y" -> Pattern.Var("y", 5 +> 1)
            ), ignoreExtra = false, 0 +> 8)
        }
        "unmentioned members ignore" - {
          pat.parse("{ x, _ }").get.value ==>
            Pattern.Struct(None, List(
              "x" -> Pattern.Var("x", 2 +> 1)
            ), ignoreExtra = true, 0 +> 8)
        }
      }
      "Alternative pattern" - {
        pat.parse("1 | 2 | 3").get.value ==>
          Pattern.Or(
            Pattern.Or(
              Pattern.IntLit(1, 0 +> 1),
              Pattern.IntLit(2, 4 +> 1),
              0 +> 5
            ),
            Pattern.IntLit(3, 8 +> 1),
            0 +> 9
          )
      }
      "Pattern in parens" - {
        pat.parse("(1 | 2 | 3)").get.value ==>
          Pattern.Or(
            Pattern.Or(
              Pattern.IntLit(1, 1 +> 1),
              Pattern.IntLit(2, 5 +> 1),
              1 +> 5
            ),
            Pattern.IntLit(3, 9 +> 1),
            0 +> 11
          )
      }
      "Alias" - {
        pat.parse("x @ _").get.value ==>
          Pattern.Alias("x", Pattern.Ignore(4 +> 1), 0 +> 5)
      }
      "Pin" - {
        pat.parse("^x").get.value ==>
          Pattern.Pin(Var("x", 1 +> 1), 0 +> 2)
      }
    }

    "definition should parse" - {
      val defn = p.whole(p.definition)
      "a function definition" - {
        "plain" - {
          defn.parse("def foo(i: i32): i32 = i + 2").get.value ==>
            DefDef("foo",
              Some(List(Param("i", TypeName.Named("i32")))),
              Some(TypeName.Named("i32")),
              InfixAp(InfixOp.Add, Var("i", 23 +> 1), IntLit(2, 27 +> 1), 23 +> 5))
        }
        "extern" - {
          defn.parse("def foo(i: i32): i32 = extern").get.value ==>
            DefDef("foo",
              Some(List(Param("i", TypeName.Named("i32")))),
              Some(TypeName.Named("i32")),
              Extern(23 +> 6))
        }
      }
      "a variable definition" - {
        defn.parse("var foo = 10").get.value ==> VarDef("foo", None, IntLit(10, 10 +> 2))
        defn.parse("var foo: i32 = 10").get.value ==> VarDef("foo", Some(TypeName.Named("i32")), IntLit(10, 15 +> 2))
      }
      "a value binding" - {
        defn.parse("val foo = 10").get.value ==> ValDef("foo", None, IntLit(10, 10 +> 2))
        defn.parse("val foo: i32 = 10").get.value ==> ValDef("foo", Some(TypeName.Named("i32")), IntLit(10, 15 +> 2))
      }
      "a struct definition" - {
        defn.parse(
          """struct foo = {
            |  x: i32
            |  y: u16; z: bool
            |}
          """.stripMargin).get.value ==>
          StructDef("foo", None, List(
            StructMemberDef("x", TypeName.Named("i32")),
            StructMemberDef("y", TypeName.Named("u16")),
            StructMemberDef("z", TypeName.Named("bool"))
          ))
        defn.parse(
          """struct arr[x] = {
            |  length: usize
            |  contents: ptr[x]
            |}
          """.stripMargin).get.value ==>
          StructDef("arr", Some(List("x")), List(
            StructMemberDef("length", TypeName.Named("usize")),
            StructMemberDef("contents", TypeName.App(TypeName.Named("ptr"), List(TypeName.Named("x"))))
          ))
      }
      "an enum definition" - {
        defn.parse(
          """enum bool = {
            |  True
            |  False
            |}
          """.stripMargin).get.value ==>
          EnumDef("bool", None, List(
            EnumVariantDef("True", Nil),
            EnumVariantDef("False", Nil)
          ))
        defn.parse(
          """enum Option[A] = {
            |  Some {
            |    value: A
            |  }
            |  None
            |}
          """.stripMargin).get.value ==>
          EnumDef("Option", Some(List("A")), List(
            EnumVariantDef("Some", List(StructMemberDef("value", TypeName.Named("A")))),
            EnumVariantDef("None", Nil)
          ))
      }
      "a type alias" - {
        defn.parse("type unit = u0").get.value ==> TypeAliasDef("unit", None, TypeName.Named("u0"))
        defn.parse("type id[a] = a").get.value ==> TypeAliasDef("id", Some(List("a")), TypeName.Named("a"))
      }
    }

    "type should parse" - {
      val tpe = p.whole(p.typ)
      "a simple named type" - {
        tpe.parse("i32").get.value ==> TypeName.Named("i32")
      }
      "a type constructor application" - {
        tpe.parse("ptr[i8]").get.value ==> TypeName.App(TypeName.Named("ptr"), List(TypeName.Named("i8")))
      }
      "an int singleton type" - {
        tpe.parse("42").get.value ==> TypeName.IntLiteral(42)
        tpe.parse("arr[i32, 10]").get.value ==>
          TypeName.App(TypeName.Named("arr"), List(TypeName.Named("i32"), TypeName.IntLiteral(10)))
      }
      "Fun" - {
        "single parameter without parens" - {
          tpe.parse("i32 => i32").get.value ==>
            TypeName.Fun(List(TypeName.Named("i32")), TypeName.Named("i32"))
        }
        "curried" - {
          "without parens" - {
            tpe.parse("i32 => i16 => i64").get.value ==>
              TypeName.Fun(List(TypeName.Named("i32")), TypeName.Fun(List(TypeName.Named("i16")), TypeName.Named("i64")))
          }
          "with parens" - {
            tpe.parse("(i32) => (i16) => i64").get.value ==>
              TypeName.Fun(List(TypeName.Named("i32")), TypeName.Fun(List(TypeName.Named("i16")), TypeName.Named("i64")))
          }
        }
        "single parameter with parens" - {
          tpe.parse("(i32) => i32").get.value ==>
            TypeName.Fun(List(TypeName.Named("i32")), TypeName.Named("i32"))
        }
        "multiple parameters" - {
          tpe.parse("(i32, i32) => i32").get.value ==>
            TypeName.Fun(List(TypeName.Named("i32"), TypeName.Named("i32")), TypeName.Named("i32"))
        }
      }
    }

    "top-level definition should parse" - {
      val tld = p.whole(p.tld)
      "a module specifier" - {
        tld.parse("module foo.bar").get.value ==> Module(List("foo", "bar"))
      }
      "an import" - {
        tld.parse("import foo._").get.value ==> Import(List("foo"), ImportSpecifier.All)
        tld.parse("import foo.bar").get.value ==> Import(List("foo"), ImportSpecifier.Members(List("bar")))
        tld.parse("import foo.{ bar, baz }").get.value ==> Import(List("foo"), ImportSpecifier.Members(List("bar", "baz")))
      }
    }

    "WS" - {
      val ws = p.whole(p.WS)
      "line comment" - {
        ws.parse("  // asdeef\n").get
      }
      "block comment" - {
        ws.parse(
          """/*
            | * this is a block comment
            | / it's neat
            | and it ends with */
            |
          """.stripMargin).get
      }
      "nested block comments" - {
        ws.parse("""/*
          |Cool
          |/*
          |block comments can be nested
          |*/*/
        """.stripMargin).get
      }
    }
  }

}
