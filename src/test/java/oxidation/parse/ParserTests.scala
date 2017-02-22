package oxidation
package parse

import ast._
import utest._

/**
  * Created by Kamil on 15.02.2017.
  */
object ParserTests extends TestSuite {

  val tests = apply {
    val p = new Parser
    "expression should parse: " - {
      "int literals" - {
        p.expression.parse("42").get.value ==> IntLit(42)
        p.expression.parse("0xdeaf").get.value ==> IntLit(0xdeaf)
      }
      "addition" - {
        p.expression.parse("2 + 3").get.value ==> InfixApply(InfixOp.Add, IntLit(2), IntLit(3))
        p.expression.parse("2 + 3 + 4").get.value ==>
          InfixApply(InfixOp.Add, InfixApply(InfixOp.Add, IntLit(2), IntLit(3)), IntLit(4))
      }
      "multiplication" - {
        p.expression.parse("2 * 3").get.value ==> InfixApply(InfixOp.Mul, IntLit(2), IntLit(3))
      }
      "mixed priorities" - {
        p.expression.parse("2 + 3 * 4").get.value ==>
          InfixApply(InfixOp.Add, IntLit(2), InfixApply(InfixOp.Mul, IntLit(3), IntLit(4)))
      }
    }
  }

}
