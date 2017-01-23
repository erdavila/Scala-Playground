package monads

object MaybeMonad extends MonadCompanion {
  override protected type M[A] = MaybeMonad[A]
  import implicits._

  def `return`[A](a: A): M[A] = Just(a)

  def mzero[A]: M[A] = Nothing

  object implicits {
    type Just[+A] = Some[A]
    val Just = Some
    val Nothing = None

    implicit class MaybeMonadWrapper[A](ma: M[A]) extends Monad[A] {
      override protected type M[A] = MaybeMonad.M[A]

      def >>=[B](f: A => M[B]): M[B] = ma match {
        case Just(a) => f(a)
        case Nothing => Nothing
      }

      def _companion = MaybeMonad
    }
  }
}

object MaybeMonadTest extends App {
  import MaybeMonad.implicits._
  import Monad.implicits._

  assert(MaybeMonad.`return`(7) == Just(7))

  assert {
    val m = Just(7) >>= { x => Just(x / 2.0) }
    m == Just(3.5)
  }

  assert {
    val m = Nothing >>= { x: Int => Just(x / 2.0) }
    m == Nothing
  }

  assert(Just(7).fmap(x => x / 2.0) == Just(3.5))

  assert {
    val m = Just(7)
    val mm = Just(m)
    mm.join() == Just(7)
  }

  assert {
    val plus4 = { x: Int => Just(4L + x) }
    val half = { x: Long => MaybeMonad.`return`(x / 2.0) } // Should work with Just instead of MaybeMonad.`return`  :-/
    val h = plus4 >=> half
    h(3) == Just(3.5)
  }

  {
    val add = MaybeMonad.liftM2 { (x: Int, y: Long) => x / y.toDouble }
    assert(add(Just(7), Just(2L)) == Just(3.5))
    assert(add(Just(7), Nothing ) == Nothing)
    assert(add(Nothing, Just(2L)) == Nothing)
  }

  assert {
    val zero: MaybeMonad[Int] = MaybeMonad.mzero
    zero == Nothing
  }

  {
    def add[A](mx: MaybeMonad[A], my: MaybeMonad[A])(implicit numeric: Numeric[A]): MaybeMonad[A] =
      for {
        x <- mx
        y <- my
      } yield numeric.plus(x, y)
    assert(add(Just(3), Just(4)) == Just(7))
    assert(add(Just(3), Nothing) == Nothing)
    assert(add(Nothing, Just(4)) == Nothing)
  }

  {
    implicit class SafelyDivisibleMaybeMonad[A](x: MaybeMonad[A])(implicit integral: Integral[A]) {
      def / (y: MaybeMonad[A]): MaybeMonad[A] =
        x >>= { a =>
          y >>= { b =>
            if (b == integral.zero) Nothing else Just(integral.quot(a, b))
          }
        }
    }

    assert(Just(10) / Just(5) == Just(2))
    assert(Just(10) / Just(0) == Nothing)
    assert(Just(10) / Just(0) / Just(2) == Nothing)
  }

  println("OK")
}
