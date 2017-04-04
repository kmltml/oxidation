package oxidation
package parse

import ast._
import fastparse.core.Parsed
import utest._

object ParserTests extends TestSuite {

  def assertFail(res: Parsed[_, _, _]): Unit = assertMatch(res) {
    case _: Parsed.Failure[_, _] =>
  }

  implicit def unresolvedSymbol(n: String): Symbol = Symbol.Unresolved(n)

  val tests = apply {
    val p = new Parser
    "expression should parse: " - {
      val expr = p.whole(p.expression)
      "int literals" - {
        expr.parse("42").get.value ==> IntLit(42)
        expr.parse("0xdeaf").get.value ==> IntLit(0xdeaf)
      }
      "bool literals" - {
        expr.parse("true").get.value ==> BoolLit(true)
        expr.parse("false").get.value ==> BoolLit(false)
      }
      "char literals" - {
        expr.parse("'a'").get.value ==> CharLit('a')
        expr.parse("'0'").get.value ==> CharLit('0')
        expr.parse("'\\n'").get.value ==> CharLit('\n')
        expr.parse("'\\\\'").get.value ==> CharLit('\\')
        expr.parse("'\\''").get.value ==> CharLit('\'')
        expr.parse("'\\0'").get.value ==> CharLit('\0')
      }
      "string literals" - {
        expr.parse(""" "Hello, \"world\"!\n" """).get.value ==> StringLit("Hello, \"world\"!\n")
      }
      "struct literals" - {
        expr.parse(
          """Vector {
            |  x = 10
            |  y = 20
            |}
          """.stripMargin).get.value ==> StructLit("Vector", List(
            "x" -> IntLit(10),
            "y" -> IntLit(20)
          ))
        expr.parse(
          "Vector { x = 10, y = 20 }").get.value ==> StructLit("Vector", List(
            "x" -> IntLit(10),
            "y" -> IntLit(20)
          ))
      }
      "unit literal" - {
        expr.parse("()").get.value ==> UnitLit()
      }
      "addition" - {
        expr.parse("2 + 3").get.value ==> InfixAp(InfixOp.Add, IntLit(2), IntLit(3))
        expr.parse("2 + 3 + 4").get.value ==>
          InfixAp(InfixOp.Add, InfixAp(InfixOp.Add, IntLit(2), IntLit(3)), IntLit(4))
      }
      "multiplication" - {
        expr.parse("2 * 3").get.value ==> InfixAp(InfixOp.Mul, IntLit(2), IntLit(3))
      }
      "mixed priorities" - {
        expr.parse("2 + 3 * 4").get.value ==>
          InfixAp(InfixOp.Add, IntLit(2), InfixAp(InfixOp.Mul, IntLit(3), IntLit(4)))
      }
      "var access" - {
        for (name <- Seq("foo", "foo2", "Bar", "foo_bar", "$foobar", "_foo", "x", "if_", "definition")) {
          expr.parse(name).get.value ==> Var(name)
        }
        assertFail(expr.parse("foo bar"))
        assertFail(expr.parse("if"))
      }
      "simple arithmetic involving vars" - {
        expr.parse("i + 1").get.value ==>
          InfixAp(InfixOp.Add, Var("i"), IntLit(1))
        expr.parse("foo >= bar").get.value ==>
          InfixAp(InfixOp.Geq, Var("foo"), Var("bar"))
      }
      "parenthesised expression" - {
        expr.parse("(a + b) * c").get.value ==>
          InfixAp(InfixOp.Mul, InfixAp(InfixOp.Add, Var("a"), Var("b")), Var("c"))
      }
      "a prefix operator application" - {
        expr.parse("-1").get.value ==> PrefixAp(PrefixOp.Neg, IntLit(1))
        expr.parse("~1").get.value ==> PrefixAp(PrefixOp.Inv, IntLit(1))
        expr.parse("!1").get.value ==> PrefixAp(PrefixOp.Not, IntLit(1))
        expr.parse("-1 + ~2").get.value ==>
          InfixAp(InfixOp.Add,
            PrefixAp(PrefixOp.Neg, IntLit(1)),
            PrefixAp(PrefixOp.Inv, IntLit(2)))
      }
      "a block expression" - {
        expr.parse("{ 1 }").get.value ==> Block(Vector(IntLit(1)))
        expr.parse(
          """{
            |  foo
            |  bar; baz
            |}
          """.stripMargin).get.value ==>
            Block(Vector(
              Var("foo"), Var("bar"), Var("baz")
            ))
        assertFail(expr.parse("""{ foo bar }"""))
        expr.parse(
          """{
            |  foo
            |  (bar)
            |}
          """.stripMargin).get.value ==>
            Block(Vector(
              Var("foo"), Var("bar")
            ))
        expr.parse(
          """{
            |  val x = 10
            |  x + 1
            |}
          """.stripMargin).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10)),
              InfixAp(InfixOp.Add, Var("x"), IntLit(1))
            ))
        expr.parse(
          """{
            |  val x = 10
            |  val y = 20
            |  x + y
            |}
          """.stripMargin).get.value ==>
            Block(Vector(
              ValDef("x", None, IntLit(10)),
              ValDef("y", None, IntLit(20)),
              InfixAp(InfixOp.Add, Var("x"), Var("y"))
            ))
      }
      "a function call" - {
        expr.parse("foo()").get.value ==> App(Var("foo"), Nil)
        expr.parse("foo(1, 2)").get.value ==> App(Var("foo"), List(IntLit(1), IntLit(2)))
        expr.parse("foo(1)(2)").get.value ==> App(App(Var("foo"), List(IntLit(1))), List(IntLit(2)))
      }
      "a template application" - {
        expr.parse("foo[i32]").get.value ==> TypeApp(Var("foo"), List(TypeName.Named("i32")))
        expr.parse("identity[i32](42)").get.value ==>
          App(TypeApp(Var("identity"), List(TypeName.Named("i32"))), List(IntLit(42)))
      }
      "a member access" - {
        expr.parse("foo.bar").get.value ==> Select(Var("foo"), "bar")
      }
      "an if expression" - {
        expr.parse("if(true) 1 else 0").get.value ==> If(BoolLit(true), IntLit(1), Some(IntLit(0)))
        expr.parse("if(foo) bar()").get.value ==> If(Var("foo"), App(Var("bar"), Nil), None)
      }
      "a while loop" - {
        expr.parse("while(foo) bar()").get.value ==> While(Var("foo"), App(Var("bar"), Nil))
      }
      "a variable assignment" - {
        expr.parse("foo = 42").get.value ==> Assign(Var("foo"), None, IntLit(42))
        expr.parse("foo.bar(32).baz += 6").get.value ==>
          Assign(Select(App(Select(Var("foo"), "bar"), List(IntLit(32))), "baz"), Some(InfixOp.Add), IntLit(6))

        expr.parse("foo = bar").get.value ==> Assign(Var("foo"), None, Var("bar"))
        expr.parse("foo += bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Add), Var("bar"))
        expr.parse("foo -= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Sub), Var("bar"))
        expr.parse("foo *= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Mul), Var("bar"))
        expr.parse("foo /= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Div), Var("bar"))
        expr.parse("foo %= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Mod), Var("bar"))
        expr.parse("foo <<= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Shl), Var("bar"))
        expr.parse("foo >>= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Shr), Var("bar"))
        expr.parse("foo ^= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Xor), Var("bar"))
        expr.parse("foo &= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.BitAnd), Var("bar"))
        expr.parse("foo |= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.BitOr), Var("bar"))
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
              InfixAp(InfixOp.Add, Var("i"), IntLit(2)))
        }
        "extern" - {
          defn.parse("def foo(i: i32): i32 = extern").get.value ==>
            DefDef("foo",
              Some(List(Param("i", TypeName.Named("i32")))),
              Some(TypeName.Named("i32")),
              Extern())
        }
      }
      "a variable definition" - {
        defn.parse("var foo = 10").get.value ==> VarDef("foo", None, IntLit(10))
        defn.parse("var foo: i32 = 10").get.value ==> VarDef("foo", Some(TypeName.Named("i32")), IntLit(10))
      }
      "a value binding" - {
        defn.parse("val foo = 10").get.value ==> ValDef("foo", None, IntLit(10))
        defn.parse("val foo: i32 = 10").get.value ==> ValDef("foo", Some(TypeName.Named("i32")), IntLit(10))
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
            EnumVariant("True", Nil),
            EnumVariant("False", Nil)
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
            EnumVariant("Some", List(StructMemberDef("value", TypeName.Named("A")))),
            EnumVariant("None", Nil)
          ))
      }
      "a type alias" - {
        defn.parse("type unit = u0").get.value ==> TypeAliasDef("unit", None, TypeName.Named(Symbol.Unresolved("u0")))
        defn.parse("type id[a] = a").get.value ==> TypeAliasDef("id", Some(List("a")), TypeName.Named(Symbol.Unresolved("a")))
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
