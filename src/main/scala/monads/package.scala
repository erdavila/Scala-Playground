package object monads {
  type IdentityMonad[A] = A

  type IOMonad[+A] = () => A

  type MaybeMonad[+A] = Option[A]
}
