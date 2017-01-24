package monads

object IdentityMonad extends MonadCompanion[IdentityMonad] {
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
  import Monad.implicits._

  assert(IdentityMonad.`return`(7) == 7)

  assert((7 >>= (_ / 2.0)) == 3.5)

  assert((7 fmap (_ / 2.0)) == 3.5)

  assert(7.join() == 7)

  assert {
    val double = { x: Int => 2L * x }
    val square = { x: Long => x * x.toDouble }
    val h = double >=> square
    h(3) == 36.0
  }

  assert {
    val lift = IdentityMonad.liftM2 { (x: Int, y: Long) => x / y.toDouble }
    lift(7, 2L) == 3.5
  }

  assert {
    val seven = for {
      x <- 3
      y <- 4
    } yield x + y
    seven == 7
  }

  println("OK")
}
