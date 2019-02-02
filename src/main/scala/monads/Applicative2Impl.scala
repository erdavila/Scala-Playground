package monads

import scala.language.higherKinds

trait Applicative2Impl[F[_, _], X] extends Functor2Impl[F, X] {
  def pure[A](a: A): F[X, A]

  def ap[A, B](fa: F[X, A])(ff: F[X, A => B]): F[X, B]
}
