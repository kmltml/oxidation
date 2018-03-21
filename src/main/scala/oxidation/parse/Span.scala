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

object Span {

  implicit def show(implicit translator: IndexTranslator): Show[Span] = {
    case Span(file, start, end) =>
      val source = file match {
        case Some(f) => translator.sourcefiles(f)
        case None => translator.anonymousSource.get
      }
      val (startl, startc) = source.findLocation(start) |+| (1, 1)
      val (endl, endc) = source.findLocation(end) |+| (1, 1)
      val filePrefix = file.map(f => s"$f:").getOrElse("")
      if(startl == endl) s"$filePrefix$startl::$startc - $endc"
      else s"$filePrefix$startl::$startc - $endl::$endc"
  }

}
