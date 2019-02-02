package monads

import monads.Run.run
import scala.language.higherKinds

abstract class Laws[F[_]: Run] {
  def equivalent[A](fx: F[A], fy: F[A]): Boolean =
    run(fx) == run(fy)
}
