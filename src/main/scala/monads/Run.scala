package monads

import scala.language.higherKinds

trait Run[F[_]] {
  type R[A]
  def apply[A](fa: F[A]): R[A]
}

object Run {
  def run[F[_], A](fa: F[A])(implicit r: Run[F]): r.R[A] =
    r(fa)

  implicit def instance[F[_]]: Run[F] = new Run[F] {
    override type R[A] = F[A]
    override def apply[A](fa: F[A]): R[A] = fa
  }
}
