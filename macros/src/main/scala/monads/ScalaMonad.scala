package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait ScalaMonad[M[_], A] extends Monad[M, A] {
  def flatMap[B](f: A => M[B]): M[B] = macro MonadMacros.bindImpl

  def map[B](f: A => B): M[B] = macro MonadMacros.fmapImpl
}
