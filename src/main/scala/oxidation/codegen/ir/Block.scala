package oxidation
package codegen
package ir

case class Block(name: Name, instructions: Vector[Inst], flow: FlowControl)
