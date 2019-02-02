package monads

import scala.language.higherKinds

trait Functor[F[_]] {
  def map[A, B](fa: F[A])(f: A => B): F[B]

  final def lift[A, B](f: A => B): F[A] => F[B] =
    _.map(f)

  implicit class Ops[A](fa: F[A]) {
    def map[B](f: A => B): F[B] =
      Functor.this.map(fa)(f)
  }
}

object Functor {
  def apply[F[_]](implicit F: Functor[F]): Functor[F] = F

  class Laws[F[_]: Functor: Run] extends monads.Laws[F] {
    private val M = Functor[F]
    import M.Ops

    def checkIdentity[A](fa: F[A]): Boolean =
      equivalent(
        fa.map(identity),
        identity(fa),
      )

    def checkDistributivity[A, B, C](fa: F[A], f: A => B, g: B => C): Boolean =
      equivalent(
        fa.map(f andThen g),
        fa.map(f).map(g),
      )
  }

  def laws[F[_]: Functor: Run]: Laws[F] = new Laws[F]
}
