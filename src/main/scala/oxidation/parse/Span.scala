package oxidation
package parse

import cats._
import cats.data._
import cats.implicits._

final case class Span(file: Option[String], start: Int, end: Int) {

  override def toString: String = {
    val filePrefix = file.map(_ ++ ":").orEmpty
    s"$filePrefix$start::$end"
  }

}
