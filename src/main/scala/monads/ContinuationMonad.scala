package monads

object ContinuationMonad {

  type Continuation[A, R] = A => R

  trait Cont[A] {
    def apply[R](c: Continuation[A, R]): R
  }

  // From http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Cont.html#v:callCC
  def callCC[A, B](f: (A => Cont[B]) => Cont[A]): Cont[A] =
    new Cont[A] {
      override def apply[R](c: Continuation[A, R]): R = {
        val ff: A => Cont[B] = { a: A =>
          val contB = new Cont[B] {
            override def apply[RR](cc: Continuation[B, RR]): RR = c(a).asInstanceOf[RR]
          }
          contB
        }
        val contA: Cont[A] = f(ff)
        contA(c): R
      }
    }

  implicit val contMonad: Monad[Cont] = new Monad[Cont] {
    override def map[A, B](fa: Cont[A])(f: A => B): Cont[B] =
      new Cont[B] {
        override def apply[R](c: Continuation[B, R]): R = {
          val cc: Continuation[A, R] = { a: A =>
            val b: B = f(a)
            c(b): R
          }
          fa(cc): R
        }
      }

    override def pure[A](a: A): Cont[A] =
      new Cont[A] {
        override def apply[R](c: Continuation[A, R]): R =
          c(a)
      }

    override def ap[A, B](fa: Cont[A])(ff: Cont[A => B]): Cont[B] =
      new Cont[B] {
        override def apply[R](c: Continuation[B, R]): R = {
          val cff: Continuation[A => B, R] = { f: (A => B) =>
            val ca: Continuation[A, R] = { a: A =>
              val b = f(a)
              c(b): R
            }
            fa(ca): R
          }
          ff(cff): R
        }
      }

    override def flatMap[A, B](fa: Cont[A])(f: A => Cont[B]): Cont[B] =
      new Cont[B] {
        override def apply[R](c: Continuation[B, R]): R = {
          val cc: Continuation[A, R] = { a: A =>
            val fb: Cont[B] = f(a)
            fb(c): R
          }
          fa(cc): R
        }
      }
  }

  implicit val contMonadRun: Run[Cont] = new Run[Cont] {
    override type R[A] = A

    override def apply[A](fa: Cont[A]): R[A] =
      fa(identity)
  }
}
