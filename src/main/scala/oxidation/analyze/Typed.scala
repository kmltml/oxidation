package oxidation
package analyze

case class Typed[+E](expr: E, typ: Type)
