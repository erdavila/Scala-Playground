package object monads {
  type CollectionMonad[+A] = Traversable[A]

  type IdentityMonad[A] = A

  type IOMonad[+A] = () => A

  type MaybeMonad[+A] = Option[A]

  type ReaderMonad[-E, +A] = E => A
}
