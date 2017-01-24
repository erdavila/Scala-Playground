package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait Monad2[_M[_, _], Z, A] {
  protected type M[U, T] = _M[U, T]
  protected type MZ[T] = M[Z, T]

  def fmap[B](f: A => B): MZ[B] = macro MonadMacros.fmapImpl

  def join[B]()(implicit ev: A <:< MZ[B]): MZ[B] = macro MonadMacros.joinImpl
}

trait Monad2Companion[_M[_, _]] {
  protected type M[U, T] = _M[U, T]

  case class liftM2[A, B, C](_f: (A, B) => C) {
    def apply[Z](ma: M[Z, A], mb: M[Z, B]): M[Z, C] = macro MonadMacros.liftM2Impl[A, B]
    private[monads] val f = _f
  }
}
