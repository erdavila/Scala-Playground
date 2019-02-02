package monads

import scala.language.higherKinds

trait Functor2Impl[F[_, _], X] {
  def map[A, B](fa: F[X, A])(f: A => B): F[X, B]
}
