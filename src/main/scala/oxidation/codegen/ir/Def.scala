package oxidation
package codegen
package ir

sealed trait Def

object Def {

  final case class Fun(name: Name, params: List[Register], ret: Type, body: Vector[Block]) extends Def

}
