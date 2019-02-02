package monads

import scala.language.higherKinds

abstract class Laws2[F[_, _], X](implicit run: Run2[F, X]) {
  def equivalent[A](fx: F[X, A], fy: F[X, A]): Boolean =
    run(fx) == run(fy)
}
