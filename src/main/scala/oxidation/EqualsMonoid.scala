package oxidation

import cats._
import cats.data._
import cats.implicits._

object EqualsMonoid extends Monoid[Any] {

  override def empty: Any = ???

  override def combine(x: Any, y: Any): Any = if (x == y) x else ???

  def instance[A]: Monoid[A] = this.asInstanceOf[Monoid[A]]
}
