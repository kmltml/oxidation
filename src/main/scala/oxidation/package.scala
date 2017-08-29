import cats._
import cats.data._
import cats.implicits._
import monocle.Lens

package object oxidation {

  implicit class FunctorWithFilter[F[_] : FunctorFilter, A](fa: F[A]) {
    def withFilter(f: A => Boolean) = fa.filter(f)
  }

  type Multimap[K, V] = Map[K, Set[V]]

  implicit def monadSubState[M[_], S, T](implicit M: MonadState[M, S], lens: Lens[S, T]): MonadState[M, T] = new MonadState[M, T] {

    override def get: M[T] = M.map(M.get)(lens.get)

    override def set(s: T): M[Unit] = M.modify(lens.set(s))

    // Delegate
    override def pure[A](x: A): M[A] = M.pure(x)
    override def flatMap[A, B](fa: M[A])(f: A => M[B]): M[B] = M.flatMap(fa)(f)
    override def tailRecM[A, B](a: A)(f: A => M[Either[A,B]]): M[B] = M.tailRecM(a)(f)

  }

}

