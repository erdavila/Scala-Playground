package monads

object ContinuationMonad extends Monad2Companion {
  override protected type MM[Z, A] = ContinuationMonad[Z, A]

  def `return`[R] = new Return[R]
  class Return[R] {
    def apply[A](a: A): MM[R, A] = { f => f(a) }
  }

  def mzero[R](r: R): MM[R, Nothing] = { _ => r }

  object implicits {
    implicit class ContinuationMonadWrapper[R, A](ma: MM[R, A]) extends ScalaMonad[A] {
      override protected type M[A] = MM[R, A]

      def >>=[B](f: A => M[B]): M[B] = { k => ma(a => f(a)(k)) }

      def _companion = ContinuationMonad
    }
  }

  def callcc[R, A, B](f: (A => MM[R, B]) => MM[R, A]): MM[R, A] = {
    val m0: MM[R, A] = { k =>
      val m1: MM[R, A] = f { a =>
        val m2: MM[R, B] = x => k(a)
        m2
      }
      m1(k)
    }
    m0
  }
}

object ContinuationMonadTest extends App {
  import ContinuationMonad.implicits._
  import Monad.implicits._

  assert {
    val m = ContinuationMonad.`return`[Double](7)
    m(_ / 2.0) == 3.5
  }

  assert {
    val ma: (Int => String) => String = { g => g(7) + "A" }
    val m = ma >>= { a =>
      { h => h(a / 2.0) + "B" }: ((Double => String) => String)
    }
    m(_.toString() + "C") == "3.5CBA"
  }

  assert {
    val ma: (Int => String) => String = { g => g(7) + "A" }
    val m = ma.fmap(_ / 2.0)
    m(_.toString() + "B") == "3.5BA"
  }

  assert {
    val m: (Int => String) => String = { g => g(7) + "A" }
    val mm: (((Int => String) => String) => String) => String = { f => f(m) + "B" }
    val joined = mm.join()
    joined(_.toString() + "X") == "7XAB"
  }

  assert {
    val plus4: Int => (Long => String) => String = { a => fa => fa(a + 4L) + "A" }
    val half: Long => (Double => String) => String = { b => fb => fb(b / 2.0) + "B" }
    val h = plus4 >=> half
    h(3)(_.toString() + "C") == "3.5CBA"
  }

  assert {
    val lift = ContinuationMonad.liftM2 { (x: Int, y: Long) => x / y.toDouble }
    val ma: (Int => String) => String = { f => f(7) + "A" }
    val mb: (Long => String) => String = { g => g(2L) + "B" }
    val mc = lift(ma, mb)
    mc(_.toString() + "C") == "3.5CBA"
  }

  assert {
    val mz = ContinuationMonad.mzero("A")
    val f: Int => (Double => String) => String = { _ => ??? /* Should never execute! */ }
    val m = mz >>= f
    m(_.toString() + "B") == "A"
  }

  assert {
    val ma: (Int => String) => String = { f => f(7) + "A" }
    val mb: (Long => String) => String = { g => g(2L) + "B" }
    val mc = for {
      a <- ma
      b <- mb
    } yield a / b.toDouble
    mc(_.toString() + "C") == "3.5CBA"
  }

  assert {
    type Message = Int
    type Response = String

    val Message1: Message = 3
    val Message2: Message = 4
    val Response1: Response = "A"
    val Response2: Response = "B"
    val Result: Response = "Z"

    val traceBuilder = Seq.newBuilder[Event]
    trait Event { traceBuilder += this }
    case class Begin(location: String) extends Event
    case class Received[T](location: String, value: T) extends Event
    case class Called(location: String, function: String) extends Event

    val trace = {
      val monad0 = ContinuationMonad.callcc[Response, Message, Nothing] { send =>
        Begin("callcc")

        val monad1 = send(Message1)
        Called("callcc", "send")

        val response = monad1 { ? => Received("monad1", ?); ??? }
        Received("callcc", response)

        val monad2 = { send: (Message => Response) =>
          Begin("monad2")

          val response = send(Message2)
          Received("monad2", response)

          Result
        }
        monad2
      }
      Called("outside", "callcc")

      val communicate = monad0
      val result = communicate { message =>
        Received("communicate", message)

        message match {
          case Message1 => Response1
          case Message2 => Response2
        }
      }
      Received("outside", result)

      traceBuilder.result()
    }

    trace == Seq(
      Called("outside", "callcc"),
      Begin("callcc"),
      Called("callcc", "send"),
      Received("communicate", Message1),
      Received("callcc", Response1),
      Begin("monad2"),
      Received("communicate", Message2),
      Received("monad2", Response2),
      Received("outside", Result)
    )
  }

  println("OK")
}
