package monads

import scala.language.experimental.macros
import scala.language.higherKinds

trait ScalaMonad2[M[_, _], Z, A] extends Monad2[M, Z, A] {
  def flatMap[B](f: A => M[Z, B]): M[Z, B] = macro MonadMacros.bindImpl

  def map[B](f: A => B): M[Z, B] = macro MonadMacros.fmapImpl
}
