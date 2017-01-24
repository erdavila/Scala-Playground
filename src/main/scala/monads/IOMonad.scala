package monads

import scala.collection.mutable.ArrayBuffer

object IOMonad extends MonadCompanion[IOMonad] {
  def `return`[A](a: A): M[A] = () => a

  object implicits {
    implicit class IOMonadWrapper[A](ma: M[A]) extends ScalaMonad[M, A] {
      def >>=[B](f: A => M[B]): M[B] = { () =>
        val a = ma()
        val mb = f(a)
        mb()
      }

      def >>[B](mb: M[B]): M[B] = { () =>
        val m = ma >>= { _ => mb }
        m()
      }

      def _companion = IOMonad
    }
  }
}

object IOMonadTest extends App {
  import IOMonad.implicits._
  import Monad.implicits._

  assert {
    val unit = IOMonad.`return`(7)
    unit() == 7
  }

  object SideEffect {
    private class Context {
      private[SideEffect] val values = ArrayBuffer.empty[Any]

      def Monad[A](a: A) = { () =>
        values += a
        a
      }

      def ioMonadMatcher = new Object {
        override def equals(that: Any): Boolean = that match {
          case _: Function0[_] => true
          case _ => false
        }
      }
    }

    def assert(f: Context => Seq[Any]): Unit = {
      val ctx = new Context
      val values = f(ctx)
      scala.Predef.assert(values == ctx.values, values + " != " + ctx.values)
    }
  }

  SideEffect.assert { ctx =>
    val m = ctx.Monad(7)
    val bound = m >>= { x => ctx.Monad(x / 2.0) }
    assert(bound() == 3.5)
    Seq(7, 3.5)
  }

  SideEffect.assert { ctx =>
    val m = ctx.Monad(7)
    val half = m fmap { x => x / 2.0 }
    assert(half() == 3.5)
    Seq(7)
  }

  SideEffect.assert { ctx =>
    val m: IOMonad[Int] = ctx.Monad(7)
    val mm: IOMonad[IOMonad[Int]] = ctx.Monad(m)
    val joined: IOMonad[Int] = mm.join()
    assert(joined() == 7)
    Seq(ctx.ioMonadMatcher, 7)
  }

  def putStrLn(msg: String) = { () => println(msg) }
  def getLine = { () => scala.io.StdIn.readLine() }

  {
    val question: Char => IOMonad[String] = { prompt: Char =>
      println("Type something " + prompt)
      getLine
    }
    val answer: String => IOMonad[Unit] = putStrLn _

    val h = (question >=> answer)('>')
    h()
  }

  {
    val main =
      putStrLn("What is your name?") >>
      getLine >>= { name =>
        putStrLn("Nice to meet you, " ++ name ++ "!")
      }
    main()
  }

  {
    val main = for {
      _ <- putStrLn("What is your name?")
      name <- getLine
      _ <- putStrLn("Nice to meet you, " ++ name ++ "!")
    } yield ()
    main()
  }

  {
    val echo = getLine >>= putStrLn
    echo()
  }

  println("OK")
}
