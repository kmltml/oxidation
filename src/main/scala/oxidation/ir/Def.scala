package oxidation
package ir

import oxidation.codegen.Name

sealed trait Def

object Def {

  final case class Fun(name: Name, params: List[Register], ret: Type, body: Vector[Block]) extends Def

}
