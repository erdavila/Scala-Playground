package monads

import scala.language.higherKinds

trait Monad[F[_]] extends Applicative[F] {
  final def unit[A](a: A): F[A] = pure(a)

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  final def flatten[A](ffa: F[F[A]]): F[A] =
    flatMap(ffa)(identity)

  implicit class Ops[A](fa: F[A]) extends super.Ops[A](fa) {
    def flatMap[B](f: A => F[B]): F[B] =
      Monad.this.flatMap(fa)(f)

    def flatten[B](implicit ev: F[A] => F[F[B]]): F[B] =
      Monad.this.flatten(fa)
  }
}

object Monad {
  def apply[F[_]](implicit M: Monad[F]): Monad[F] = M

  class Laws[F[_]: Monad] {
    private val M = Monad[F]
    import M.Ops

    def checkLeftIdentity[A, B](a: A, f: A => F[B]): Boolean =
      M.unit(a).flatMap(f) == f(a)

    def checkRightIdentity[A](ma: F[A]): Boolean =
      ma.flatMap(M.unit) == ma

    def checkAssociativity[A, B, C](fa: F[A], f: A => F[B], g: B => F[C]): Boolean =
      fa.flatMap(f).flatMap(g) == fa.flatMap { a => f(a).flatMap(g) }
  }

  def laws[F[_]: Monad]: Laws[F] = new Laws[F]
}
