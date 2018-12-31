package monads

object IOMonad {
  sealed trait IO[A] {
    def run: A
  }

  def io[A](a: => A): IO[A] = new IO[A] {
    override def run: A = a
  }

  implicit val ioMonad: Monad[IO] = new Monad[IO] {
    override def map[A, B](fa: IO[A])(f: A => B): IO[B] =
      new IO[B] {
        override def run: B =
          f(fa.run)
      }

    override def pure[A](a: A): IO[A] =
      io(a)

    override def ap[A, B](fa: IO[A])(ff: IO[A => B]): IO[B] =
      new IO[B] {
        override def run: B =
          ff.run(fa.run)
      }

    override def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] =
      new IO[B] {
        override def run: B =
          f(fa.run).run
      }
  }

  implicit val ioMonadRun: Run[IO] = new Run[IO] {
    override type R[A] = A

    override def apply[A](fa: IO[A]): R[A] =
      fa.run
  }
}
