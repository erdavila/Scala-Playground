package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait MonadCompanion {
  protected type M[_]

  case class liftM2[A, B, C](_f: (A, B) => C) {
    private[monads] def f = _f

    def apply(ma: M[A], mb: M[B]): M[C] = macro MonadMacros.liftM2Impl[A, B]
  }
}
