package monads

import scala.language.higherKinds

trait Run2[F[_, _], X] {
  type R[A]
  def apply[A](fa: F[X, A]): R[A]
}

object Run2 {
  def run[F[_, _], X, A](fa: F[X, A])(implicit r: Run2[F, X]): r.R[A] =
    r(fa)

  implicit def instance[F[_, _], X]: Run2[F, X] = new Run2[F, X] {
    override type R[A] = F[X, A]
    override def apply[A](fa: F[X, A]): R[A] = fa
  }
}
