package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait Monad2Companion {
  protected type MM[_, _]

  case class liftM2[A, B, C](_f: (A, B) => C) {
    private[monads] val f = _f

    def apply[Z](ma: MM[Z, A], mb: MM[Z, B]): MM[Z, C] = macro MonadMacros.liftM2Impl[A, B]
  }
}
