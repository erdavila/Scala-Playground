package monads

object IdentityMonad extends MonadCompanion {
  override protected type M[A] = IdentityMonad[A]

  def `return`[A](a: A): M[A] = a

  object implicits {
    implicit class IdentityMonadWrapper[A](ma: M[A]) extends ScalaMonad[A] {
      override protected type M[T] = IdentityMonad.M[T]

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
    val plus4 = { x: Int => 4L + x }
    val half = { x: Long => x / 2.0 }
    val h = plus4 >=> half
    h(3) == 3.5
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
