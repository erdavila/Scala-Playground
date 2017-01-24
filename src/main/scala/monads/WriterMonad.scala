package monads

object WriterMonad extends Monad2Companion[WriterMonad] {
  trait Monoid[W] {
    def identity: W
    def op(a: W, b: W): W
  }

  def `return`[W, A](a: A)(implicit monoid: Monoid[W]): M[W, A] = (monoid.identity, a)

  object implicits {
    implicit class WriterMonadWrapper[W, A](ma: M[W, A]) extends ScalaMonad2[M, W, A] {
      def >>=[B](f: A => M[W, B])(implicit monoid: Monoid[W]): M[W, B] = {
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
    val m: (String, Int) = WriterMonad.`return`(7)
    m == ("", 7)
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
    val double = { x: Int => ("double", 2L * x) }
    val square = { x: Long => ("square", x * x.toDouble) }
    val h = double >=> square
    h(3) == ("double|square", 36.0)
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
