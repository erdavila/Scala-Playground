package monads

object IdentityMonad {
  type Identity[A] = A

  implicit val identityMonad: Monad[Identity] = new Monad[Identity] {
    override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] =
      f(fa)

    override def pure[A](a: A): Identity[A] =
      a

    override def ap[A, B](fa: Identity[A])(ff: Identity[A => B]): Identity[B] =
      ff(fa)

    override def flatMap[A, B](fa: Identity[A])(f: A => Identity[B]): Identity[B] =
      f(fa)
  }
}
