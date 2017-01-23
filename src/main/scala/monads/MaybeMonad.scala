package monads

object MaybeMonad extends MonadCompanion[MaybeMonad] {
  def `return`[A](a: A): M[A] = implicits.Just(a)

  def mzero[A]: M[A] = implicits.Nothing

  object implicits {
    type Just[+A] = Some[A]
    val Just = Some
    val Nothing = None

    implicit class MaybeMonadWrapper[A](ma: M[A]) extends Monad[M, A] {
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
    val m = Just(7) >>= { x => MaybeMonad.`return`(x / 2.0) }
    m == Just(3.5)
  }

  assert {
    val m = Nothing >>= { x: Int => MaybeMonad.`return`(x / 2.0) }
    m == Nothing
  }

  assert(Just(3).fmap(x => x * 1.5) == Just(4.5))

  assert(Just(Just(3)).join() == Just(3))

  assert {
    val double = { x: Int => MaybeMonad.`return`(2L * x) }
    val square = { x: Long => MaybeMonad.`return`(x * x.toDouble) }
    val h = double >=> square
    h(3) == Just(36.0)
  }

  {
    val add = MaybeMonad.liftM2((_: Int) + (_: Long).toDouble)
    assert(add(Just(3), Just(4L)) == Just(7.0))
    assert(add(Just(3), Nothing)  == Nothing)
    assert(add(Nothing, Just(4L)) == Nothing)
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
