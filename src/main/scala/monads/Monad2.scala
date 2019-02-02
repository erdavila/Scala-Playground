package monads

import scala.language.higherKinds

class Monad2[F[_, _]] extends Applicative2[F] {
  final def unit[X, A](a: A)(implicit impl: Monad2Impl[F, X]): F[X, A] =
    pure(a)

  final def flatMap[X, A, B](fa: F[X, A])(f: A => F[X, B])(implicit impl: Monad2Impl[F, X]): F[X, B] =
    impl.flatMap(fa)(f)

  final def flatten[X, A](ffa: F[X, F[X, A]])(implicit impl: Monad2Impl[F, X]): F[X, A] =
    flatMap(ffa)(identity)

  implicit class Ops[X, A](fa: F[X, A])(implicit impl: Monad2Impl[F, X]) extends super.Ops[X, A](fa) {
    def flatMap[B](f: A => F[X, B]): F[X, B] =
      Monad2.this.flatMap(fa)(f)

    def flatten[B](implicit ev: F[X, A] => F[X, F[X, B]]): F[X, B] =
      Monad2.this.flatten(fa)
  }
}

object Monad2 {
  implicit def apply[F[_, _]]: Monad2[F] = new Monad2[F]

  class Laws[F[_, _], X](implicit impl: Monad2Impl[F, X], run: Run2[F, X]) extends monads.Laws2[F, X] {
    private val M = Monad2[F]
    import M.Ops

    def checkLeftIdentity[A, B](a: A, f: A => F[X, B]): Boolean =
      equivalent(
        M.unit(a).flatMap(f),
        f(a),
      )

    def checkRightIdentity[A](ma: F[X, A]): Boolean =
      equivalent(
        ma.flatMap(M.unit[X, A]),
        ma,
      )

    def checkAssociativity[A, B, C](fa: F[X, A], f: A => F[X, B], g: B => F[X, C]): Boolean =
      equivalent(
        fa.flatMap(f).flatMap(g),
        fa.flatMap { a => f(a).flatMap(g) },
      )
  }

  def laws[F[_, _], X](implicit impl: Monad2Impl[F, X], run: Run2[F, X]): Laws[F, X] = new Laws[F, X]
}
