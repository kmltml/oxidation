
package object oxidation {

  implicit class FunctorOps[F[_], A](private val a: F[A])(implicit F: Functor[F]) {

    def map[B](f: A => B): F[B] = F.map(a, f)

    def mapTo[B](b: => B): F[B] = map(_ => b)

    def void: F[Unit] = mapTo(())

  }

}
