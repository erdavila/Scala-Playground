package monads

object CollectionMonad extends MonadCompanion[CollectionMonad] {
  def `return`[A](a: A): M[A] = Traversable(a)

  def mzero[A]: M[A] = Traversable.empty[A]

  object implicits {
    implicit class CollectionMonadWrapper[A](ma: M[A]) extends Monad[M, A] {
      def >>=[B](f: A => M[B]): M[B] =
        ma.flatMap(f)

      def _companion = CollectionMonad
    }
  }
}

object CollectionMonadTest extends App {
  import CollectionMonad.implicits._
  import Monad.implicits._

  assert(CollectionMonad.`return`(7) == Seq(7))

  assert {
    val coll = Seq(1, 2, 3) >>= { n =>
      Seq.tabulate(n) { i =>
        val c = ('A' + i).toChar
        (n, c)
      }
    }
    coll == Seq((1, 'A'), (2, 'A'), (2, 'B'), (3, 'A'), (3, 'B'), (3, 'C'))
  }

  assert {
    val coll = Seq(1, 2, 3) fmap { n =>
      val c = ('A' + n - 1).toChar
      (n, c)
    }
    coll == Seq((1, 'A'), (2, 'B'), (3, 'C'))
  }

  assert {
    val x: Seq[Int] = Seq(1, 2)
    val y: Seq[Int] = Seq(3, 4)
    val z: Seq[Seq[Int]] = Seq(x, y)
    z.join() == Seq(1, 2, 3, 4)
  }

  assert {
    val f = { x: Int => Traversable(x + 1L, x + 2L) }
    val g = { x: Long => Traversable(x / 2.0, x * 2.0) }
    val h = f >=> g
    h(7) == Seq(4.0, 16.0, 4.5, 18.0)
  }

  assert {
    val lift = CollectionMonad.liftM2((_: Int) * (_: Int))
    val lifted = lift(Traversable(2, 3), Traversable(5, 7))
    lifted == Seq(10, 14, 15, 21)
  }

  assert(CollectionMonad.mzero == Seq.empty)

  assert {
    val values = for {
      x <- Seq(2, 3)
      y <- Seq(5, 7)
    } yield x * y
    values == Seq(10, 14, 15, 21)
  }

  println("OK")
}
