package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait Monad[A] {
  protected type M[_]

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
