import cats._
import cats.data._
import cats.implicits._

package object oxidation {

  implicit class FunctorOps[F[_], A](private val a: F[A])(implicit F: oxidation.Functor[F]) {

    def map[B](f: A => B): F[B] = F.map(a, f)

    def mapTo[B](b: => B): F[B] = map(_ => b)

    def void: F[Unit] = mapTo(())

  }

  implicit class FunctorWithFilter[F[_] : FunctorFilter, A](fa: F[A]) {
    def withFilter(f: A ⇒ Boolean) = fa.filter(f)
  }

//  implicit def rightBiasedEither[L, R](either: Either[L, R]): Either.RightProjection[L, R] = either.right

}
