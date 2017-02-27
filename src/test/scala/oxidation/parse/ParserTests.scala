package oxidation
package parse

import ast._
import fastparse.core.Parsed
import utest._

object ParserTests extends TestSuite {

  def assertFail(res: Parsed[_, _, _]): Unit = assertMatch(res) {
    case _: Parsed.Failure[_, _] =>
  }

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
        expr.parse("{ 1 }").get.value ==> Block(Seq(IntLit(1)))
        expr.parse(
          """{
            |  foo
            |  bar; baz
            |}
          """.stripMargin).get.value ==>
            Block(Seq(
              Var("foo"), Var("bar"), Var("baz")
            ))
        assertFail(expr.parse("""{ foo bar }"""))
        expr.parse(
          """{
            |  foo
            |  (bar)
            |}
          """.stripMargin).get.value ==>
            Block(Seq(
              Var("foo"), Var("bar")
            ))
        expr.parse(
          """{
            |  val x = 10
            |  x + 1
            |}
          """.stripMargin).get.value ==>
            Block(Seq(
              ValDef("x", None, IntLit(10)),
              InfixAp(InfixOp.Add, Var("x"), IntLit(1))
            ))
      }
      "a function call" - {
        expr.parse("foo()").get.value ==> Apply(Var("foo"), Seq.empty)
        expr.parse("foo(1, 2)").get.value ==> Apply(Var("foo"), Seq(IntLit(1), IntLit(2)))
        expr.parse("foo(1)(2)").get.value ==> Apply(Apply(Var("foo"), Seq(IntLit(1))), Seq(IntLit(2)))
      }
      "a member access" - {
        expr.parse("foo.bar").get.value ==> Select(Var("foo"), "bar")
      }
      "an if expression" - {
        expr.parse("if(true) 1 else 0").get.value ==> If(BoolLit(true), IntLit(1), Some(IntLit(0)))
        expr.parse("if(foo) bar()").get.value ==> If(Var("foo"), Apply(Var("bar"), Seq()), None)
      }
      "a while loop" - {
        expr.parse("while(foo) bar()").get.value ==> While(Var("foo"), Apply(Var("bar"), Seq.empty))
      }
      "a variable assignment" - {
        expr.parse("foo = 42").get.value ==> Assign(Var("foo"), None, IntLit(42))
        expr.parse("foo.bar(32).baz += 6").get.value ==>
          Assign(Select(Apply(Select(Var("foo"), "bar"), Seq(IntLit(32))), "baz"), Some(InfixOp.Add), IntLit(6))

        expr.parse("foo = bar").get.value ==> Assign(Var("foo"), None, Var("bar"))
        expr.parse("foo += bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Add), Var("bar"))
        expr.parse("foo -= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Sub), Var("bar"))
        expr.parse("foo *= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Mul), Var("bar"))
        expr.parse("foo /= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Div), Var("bar"))
        expr.parse("foo %= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Mod), Var("bar"))
        expr.parse("foo <<= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Shl), Var("bar"))
        expr.parse("foo >>= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Shr), Var("bar"))
        expr.parse("foo ^= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Xor), Var("bar"))
        expr.parse("foo &= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.And), Var("bar"))
        expr.parse("foo |= bar").get.value ==> Assign(Var("foo"), Some(InfixOp.Or), Var("bar"))
      }
    }

    "definition should parse" - {
      val defn = p.whole(p.definition)
      "a function definition" - {
        defn.parse("def foo(i: i32): i32 = i + 2").get.value ==>
          DefDef("foo",
            Some(List(Param("i", Type.Named("i32")))),
            Some(Type.Named("i32")),
            InfixAp(InfixOp.Add, Var("i"), IntLit(2)))
      }
      "a variable definition" - {
        defn.parse("var foo = 10").get.value ==> VarDef("foo", None, IntLit(10))
        defn.parse("var foo: i32 = 10").get.value ==> VarDef("foo", Some(Type.Named("i32")), IntLit(10))
      }
      "a value binding" - {
        defn.parse("val foo = 10").get.value ==> ValDef("foo", None, IntLit(10))
        defn.parse("val foo: i32 = 10").get.value ==> ValDef("foo", Some(Type.Named("i32")), IntLit(10))
      }
      "a struct definition" - {
        defn.parse(
          """struct foo = {
            |  x: i32
            |  y: u16; z: bool
            |}
          """.stripMargin).get.value ==>
            StructDef("foo", Seq(
              StructMember("x", Type.Named("i32")),
              StructMember("y", Type.Named("u16")),
              StructMember("z", Type.Named("bool"))
            ))
      }
    }
  }

}
