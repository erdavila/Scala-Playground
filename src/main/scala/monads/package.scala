package object monads {
  type IOMonad[+A] = () => A

  type MaybeMonad[+A] = Option[A]
}
