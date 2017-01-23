package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait Monad[_M[_], A] {
  protected type M[T] = _M[T]

  def fmap[B](f: A => B): M[B] = macro MonadMacros.fmapImpl

  def join[B]()(implicit ev: A <:< M[B]): M[B] = macro MonadMacros.joinImpl
}

object Monad {
  object implicits {
    implicit class MonadFactoryWrapper[A, MB](_f: A => MB) {
      private[monads] def f = _f

      def >=>[B, MC](g: B => MC): A => MC = macro MonadMacros.composeImpl
    }
  }
}

trait MonadCompanion[_M[_]] {
  protected type M[T] = _M[T]

  case class liftM2[A, B, C](_f: (A, B) => C) {
    private[monads] def f = _f

    def apply(ma: M[A], mb: M[B]): M[C] = macro MonadMacros.liftM2Impl[A, B]
  }
}
