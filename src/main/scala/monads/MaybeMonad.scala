package monads

object MaybeMonad {
  def some[A](a: A): Option[A] = Some(a)
  def none[A]: Option[A] = None

  implicit val maybeMonad: Monad[Option] = new Monad[Option] {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] =
      fa.map(f)

    override def pure[A](a: A): Option[A] =
      some(a)

    override def ap[A, B](fa: Option[A])(ff: Option[A => B]): Option[B] =
      for (a <- fa; f <- ff)
        yield f(a)

    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] =
      fa.flatMap(f)
  }
}
