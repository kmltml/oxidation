import cats._
import cats.data._
import cats.implicits._

package object oxidation {

  implicit class FunctorWithFilter[F[_] : FunctorFilter, A](fa: F[A]) {
    def withFilter(f: A ⇒ Boolean) = fa.filter(f)
  }

  type Multimap[K, V] = Map[K, Set[V]]

//  implicit def rightBiasedEither[L, R](either: Either[L, R]): Either.RightProjection[L, R] = either.right

}
