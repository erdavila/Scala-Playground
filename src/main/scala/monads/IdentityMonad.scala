package monads

object IdentityMonad {
  type M[T] = IdentityMonad[T]

  def `return`[A](a: A): M[A] = a

  object implicits {
    implicit class IdentityMonadWrapper[A](ma: M[A]) extends ScalaMonad[M, A] {
      def >>=[B](f: A => M[B]): M[B] = f(ma)

      def _companion = IdentityMonad
    }
  }
}

object IdentityMonadTest extends App {
  import IdentityMonad.implicits._

  assert {
    val seven = for {
      x <- 3
      y <- 4
    } yield x + y
    seven == 7
  }

  println("OK")
}
