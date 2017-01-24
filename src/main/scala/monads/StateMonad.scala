package monads

object StateMonad extends Monad2Companion[StateMonad] {
  def `return`[S, A](a: A): M[S, A] = { s => (a, s) }

  object implicits {
    implicit class StateMonadWrapper[S, A](ma: M[S, A]) extends ScalaMonad2[M, S, A] {
      def >>=[B](f: A => M[S, B]): M[S, B] = { r =>
        val (a, s) = ma(r)
        f(a)(s)
      }

      def _companion = StateMonad
    }
  }

  def get[S] = { s: S => (s, s) }
  def put[S](s: S) = { _: S => ((), s) }
  def modify[S, T](f: (S => T)) = { s: S => ((), f(s)) }
}

object StateMonadTest extends App {
  import StateMonad.implicits._
  import Monad.implicits._

  assert {
    val m: Char => (Int, Char) = StateMonad.`return`(7)
    m('@') == (7, '@')
  }

  assert {
    val m0 = { (s: Int) => (s / 2.0, s / 2) }
    val m = m0 >>= { x => (s: Int) => (x + "|" + s, s + 1) }
    m(7) == ("3.5|3", 4)
  }

  assert {
    val m0 = { (s: Int) => (s / 2.0, s / 2) }
    val m = m0 fmap { _.toString() }
    m(7) == ("3.5", 3)
  }

  assert {
    val N = 70
    val mma = { s0: Int =>
      val ma = { s1: Int => (s0 + "|" + s1, s0 + s1) }
      (ma, s0 * N)
    }

    val m = mma.join()
    val X = 3
    m(X) == (X + "|" + (X * N), X + X * N)
  }

  assert {
    val f = { a: Char => s: String => (s.length() / 2.0, a + s) }
    val g = { b: Double => s: String => (s.length(), b + s)  }
    val m = f >=> g

    val (y, ss) = f('A')("BCD")
    m('A')("BCD") == g(y)(ss)
  }

  assert {
    val s = (_: Int) + (_: Double)
    val lift = StateMonad.liftM2(s)
    val ma = { s: String => (3, s + "|3") }
    val mb = { s: String => (4.0, s + "|4.0") }
    val m = lift(ma, mb)

    m("!") == (7.0, "!|3|4.0")
  }

  {
    type S = List[Int]
    def push(x: Int) = (s: S) => ((), x :: s)
    def top = (s: S) => (s.head, s)
    def pop = (s: S) => (s.head, s.tail)

    assert {
      val m = for {
        _     <- push(3)
        three <- top
        _     <- push(4)
        four  <- top
        _     <- push(three + four)
        seven <- pop
      } yield seven
      m(List(0)) == (7, List(4, 3, 0))
    }
  }

  assert {
    val m = for {
      s0 <- StateMonad.get[String]
      _  <- StateMonad.put("A")
      _  <- StateMonad.modify((_: String) + "B")
      sN <- StateMonad.get[String]
    } yield (s0, sN)

    m("@") == (("@", "AB"), "AB")
  }

  println("OK")
}
