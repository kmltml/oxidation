package oxidation
package ir

import cats._
import cats.data._
import cats.implicits._

sealed trait ConstantPoolEntry extends Product with Serializable

object ConstantPoolEntry {

  final case class Str(value: String) extends ConstantPoolEntry

  implicit val show: Show[ConstantPoolEntry] = {
    case Str(v) => "string \"" + v.flatMap {
      case '"' => "\\\""
      case '\n' => "\\n"
      case '\0' => "\\0"
      case c => c.toString
    } + "\""
  }

}
