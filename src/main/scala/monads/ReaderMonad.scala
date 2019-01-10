package monads

import scala.language.implicitConversions

object ReaderMonad {
  type Reader[E, A] = E => A

  object Reader {
    def apply[E, A](r: E => A): Reader[E, A] = r
  }

  implicit def toOps[E, A](r: E => A)(implicit M: Monad2[({type L[e, a] = e => a})#L]) =
    M.Ops(r)

  implicit def readerMonad2Impl[E]: Monad2Impl[Reader, E] = new Monad2Impl[Reader, E] {
    override def map[A, B](fa: Reader[E, A])(f: A => B): Reader[E, B] =
      Reader { e =>
        val a: A = fa(e)
        val b: B = f(a)
        b
      }

    override def pure[A](a: A): Reader[E, A] =
      _ => a

    override def ap[A, B](fa: Reader[E, A])(ff: Reader[E, A => B]): Reader[E, B] =
      Reader { e =>
        val f: A => B = ff(e)
        val a: A = fa(e)
        val b: B = f(a)
        b
      }

    override def flatMap[A, B](fa: Reader[E, A])(f: A => Reader[E, B]): Reader[E, B] =
      Reader { e =>
        val a: A = fa(e)
        val fb: Reader[E, B] = f(a)
        val b: B = fb(e)
        b
      }
  }

  implicit def readerMonadRun[E](implicit e: E): Run2[Reader, E]  = new Run2[Reader, E] {
    override type R[A] = A

    override def apply[A](fa: Reader[E, A]): R[A] =
      fa(e)
  }
}
