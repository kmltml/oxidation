package oxidation
package ir

import oxidation.codegen.Name

sealed trait Def

object Def {

  final case class Fun(name: Name, params: List[Register], ret: Type, body: Vector[Block], constantPool: Set[ConstantPoolEntry]) extends Def
  final case class ExternFun(name: Name, params: List[Type], ret: Type) extends Def
  final case class TrivialVal(name: Name, value: Val) extends Def

}
