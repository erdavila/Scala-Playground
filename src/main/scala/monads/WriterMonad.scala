package monads

object WriterMonad extends Monad2Companion {
  override protected type MM[Z, A] = WriterMonad[Z, A]

  trait Monoid[W] {
    def identity: W
    def op(a: W, b: W): W
  }

  def `return`[W] = new Return[W]
  class Return[W] {
    def apply[A](a: A)(implicit monoid: Monoid[W]): MM[W, A] = (monoid.identity, a)
  }

  object implicits {
    implicit class WriterMonadWrapper[W, A](ma: MM[W, A]) extends ScalaMonad[A] {
      override protected type M[A] = MM[W, A]

      def >>=[B](f: A => M[B])(implicit monoid: Monoid[W]): M[B] = {
        val (wa, a) = ma
        val (wb, b) = f(a)
        val w = monoid.op(wa, wb)
        (w, b)
      }

      def _companion = WriterMonad
    }
  }
}

object WriterMonadTest extends App {
  import WriterMonad.implicits._
  import Monad.implicits._

  implicit val stringPipeConcatMonoid: WriterMonad.Monoid[String] = new WriterMonad.Monoid[String] {
    def identity = ""

    def op(a: String, b: String) = Seq(a, b).filter(_.nonEmpty).mkString("|")
  }

  assert {
    {
      val m: (String, Int) = WriterMonad.`return`(7)
      m == ("", 7)
    }

    {
      val m = WriterMonad.`return`[String](7)
      m == ("", 7)
    }
  }

  assert {
    val m = ("BEGIN", 7) >>= { x => ("half", x / 2.0) }
    m == ("BEGIN|half", 3.5)
  }

  assert {
    val m = ("BEGIN", 7) fmap { x => x / 2.0 }
    m == ("BEGIN", 3.5)
  }

  assert {
    val m = ("out", ("in", 7)).join()
    m == ("out|in", 7)
  }

  assert {
    val plus4 = { x: Int => ("plus4", x + 4L) }
    val half = { x: Long => ("half", x / 2.0) }
    val h = plus4 >=> half
    h(3) == ("plus4|half", 3.5)
  }

  assert {
    val lift = WriterMonad.liftM2 { (x: Int, y: Long) => x / y.toDouble }
    val mx = ("x", 7)
    val my = ("y", 2L)
    lift(mx, my) == ("x|y", 3.5)
  }

  assert {
    val m = for {
      x <- ("x", 7)
      y <- ("y", 2L)
    } yield x / y.toDouble
    m == ("x|y", 3.5)
  }

  println("OK")
}
