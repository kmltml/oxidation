package oxidation
package ir
package validation

import cats._
import cats.data._
import cats.implicits._

import oxidation.codegen.Name

final case class Location(parentDef: Name, block: Name, index: Int)

object Location {

  implicit val show: Show[Location] = {
    case Location(parentDef, block, index) => show"$parentDef::$block::$index"
  }

}

