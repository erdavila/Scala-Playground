package monads

import scala.language.higherKinds

trait Monad2Impl[F[_, _], X] extends Applicative2Impl[F, X] {
  def flatMap[A, B](fa: F[X, A])(f: A => F[X, B]): F[X, B]
}
