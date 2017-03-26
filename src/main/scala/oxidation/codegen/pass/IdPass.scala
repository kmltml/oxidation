package oxidation
package codegen
package pass

import cats._
import cats.data._
import cats.implicits._

trait IdPass extends Pass {

  type F[A] = Id[A]
  val F = implicitly[Monad[Id]]
  def extract[A](f: Id[A]): A = f

}
