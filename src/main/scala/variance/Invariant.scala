package variance

class Invariant[T] {
  def get(): T = ???

  def getInvariant(): Invariant[T] = ???

  def getCovariant(): Covariant[T] = ???

  def getContravariant(): Contravariant[T] = ???

  def set(value: T): Unit = ???

  def setInvariant(value: Invariant[T]): Unit = ???

  def setCovariant(value: Covariant[T]): Unit = ???

  def setContravariant(value: Contravariant[T]): Unit = ???
}

object InvariantTest extends Values {
  val invariant = new Invariant[Something]

  invariant.get() : Any
  invariant.get() : Something
  //invariant.get() : Nothing
  //invariant.get() : Unrelated

  //invariant.getInvariant() : Invariant[Any]
  invariant.getInvariant() : Invariant[Something]
  //invariant.getInvariant() : Invariant[Nothing]
  //invariant.getInvariant() : Invariant[Unrelated]

  invariant.getCovariant() : Covariant[Any]
  invariant.getCovariant() : Covariant[Something]
  //invariant.getCovariant() : Covariant[Nothing]
  //invariant.getCovariant() : Covariant[Unrelated]

  //invariant.getContravariant() : Contravariant[Any]
  invariant.getContravariant() : Contravariant[Something]
  invariant.getContravariant() : Contravariant[Nothing]
  //invariant.getContravariant() : Contravariant[Unrelated]

  //invariant.set(any)
  invariant.set(something)
  invariant.set(nothing)
  //invariant.set(unrelated)

  //invariant.setInvariant(new Invariant[Any])
  invariant.setInvariant(new Invariant[Something])
  //invariant.setInvariant(new Invariant[Nothing])
  //invariant.setInvariant(new Invariant[Unrelated])

  //invariant.setCovariant(new Covariant[Any])
  invariant.setCovariant(new Covariant[Something])
  invariant.setCovariant(new Covariant[Nothing])
  //invariant.setCovariant(new Covariant[Unrelated])

  invariant.setContravariant(new Contravariant[Any])
  invariant.setContravariant(new Contravariant[Something])
  //invariant.setContravariant(new Contravariant[Nothing])
  //invariant.setContravariant(new Contravariant[Unrelated])

  //new Invariant[Any]       : Invariant[Something]
  new Invariant[Something] : Invariant[Something]
  //new Invariant[Nothing]   : Invariant[Something]
  //new Invariant[Unrelated] : Invariant[Something]
}
