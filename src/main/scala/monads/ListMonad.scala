package monads

object ListMonad {
  implicit val listMonad: Monad[List] = new Monad[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] =
      fa.map(f)

    override def pure[A](a: A): List[A] =
      List(a)

    override def ap[A, B](fa: List[A])(ff: List[A => B]): List[B] =
      for (f <- ff; a <- fa) yield f(a)

    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] =
      fa.flatMap(f)
  }
}
