package monads

import scala.language.higherKinds

class Functor2[F[_, _]] {
  final def map[X, A, B](fa: F[X, A])(f: A => B)(implicit impl: Functor2Impl[F, X]): F[X, B] =
    impl.map(fa)(f)

  final def lift[X, A, B](f: A => B)(implicit impl: Functor2Impl[F, X]): F[X, A] => F[X, B] =
    _.map(f)

  implicit class Ops[X, A](fa: F[X, A])(implicit impl: Functor2Impl[F, X]) {
    def map[B](f: A => B): F[X, B] =
      Functor2.this.map(fa)(f)
  }
}

object Functor2 {
  implicit def apply[F[_, _]]: Functor2[F] = new Functor2[F]

  class Laws[F[_, _]: Functor2, X](implicit impl: Functor2Impl[F, X], run: Run2[F, X]) extends monads.Laws2[F, X] {
    private val M = Functor2[F]
    import M.Ops

    def checkIdentity[A](fa: F[X, A]): Boolean =
      equivalent(
        fa.map(identity),
        identity(fa),
      )

    def checkDistributivity[A, B, C](fa: F[X, A], f: A => B, g: B => C): Boolean =
      equivalent(
        fa.map(f andThen g),
        fa.map(f).map(g),
      )
  }

  def laws[F[_, _], X](implicit impl: Functor2Impl[F, X], run: Run2[F, X]): Laws[F, X] = new Laws[F, X]
}
