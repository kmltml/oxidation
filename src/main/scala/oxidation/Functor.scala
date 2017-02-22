package oxidation

/**
  * Created by Kamil on 22.02.2017.
  */
trait Functor[F[_]] {

  def map[A, B](a: F[A], f: A => B): F[B]

}
