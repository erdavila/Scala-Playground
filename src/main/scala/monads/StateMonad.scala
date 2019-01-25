package monads

object StateMonad {
  type State[S, A] = S => (A, S)

  object State {
    def apply[S, A](s: S => (A, S)): State[S, A] = s
  }

  def get[S]: State[S, S] = State[S, S](s => (s, s))
  def put[S](s: S): State[S, Unit] = State[S, Unit](_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = State(s => ((), f(s)))

  implicit def stateMonad2Impl[S]: Monad2Impl[State, S] = new Monad2Impl[State, S] {
    override def map[A, B](fa: State[S, A])(f: A => B): State[S, B] =
      State { s =>
        val (a, sa) = fa(s)
        val b = f(a)
        (b, sa)
      }

    override def pure[A](a: A): State[S, A] =
      State { s =>
        (a, s)
      }

    override def ap[A, B](fa: State[S, A])(ff: State[S, A => B]): State[S, B] =
      State { s =>
        val (f, sf) = ff(s)
        val (a, sa) = fa(sf)
        val b = f(a)
        (b, sa)
      }

    override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] =
      State { s =>
        val (a, sa) = fa(s)
        f(a)(sa)
      }
  }

  implicit def stateMonadRun[S](implicit s: S): Run2[State, S]  = new Run2[State, S] {
    override type R[A] = (A, S)

    override def apply[A](fa: State[S, A]): R[A] =
      fa(s)
  }
}
