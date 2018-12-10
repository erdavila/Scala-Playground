package variance

class Covariant[+T] {
  def get(): T = ???

  //def getInvariant(): Invariant[T] = ???
  def getInvariantUpper[U >: T](): Invariant[U] = ???

  def getCovariant(): Covariant[T] = ???

  //def getContravariant(): Contravariant[T] = ???
  def getContravariantUpper[U >: T](): Contravariant[U] = ???

  //def set(value: T): Unit = ???
  def setUpper[U >: T](value: U): Unit = ???

  //def setInvariant(value: Invariant[T]): Unit = ???
  def setInvariantUpper[U >: T](value: Invariant[U]): Unit = ???

  //def setCovariant(value: Covariant[T]): Unit = ???
  def setCovariantUpper[U >: T](value: Covariant[U]): Unit = ???

  def setContravariant(value: Contravariant[T]): Unit = ???
}

object CovariantTest extends Values {
  val covariant = new Covariant[Something]

  covariant.get() : Any
  covariant.get() : Something
  //covariant.get() : Nothing
  //covariant.get() : Unrelated

  covariant.getInvariantUpper() : Invariant[Any]
  covariant.getInvariantUpper() : Invariant[Something]
  //covariant.getInvariantUpper() : Invariant[Nothing]
  //covariant.getInvariantUpper() : Invariant[Unrelated]

  covariant.getCovariant() : Covariant[Any]
  covariant.getCovariant() : Covariant[Something]
  //covariant.getCovariant() : Covariant[Nothing]
  //covariant.getCovariant() : Covariant[Unrelated]

  covariant.getContravariantUpper() : Contravariant[Any]
  covariant.getContravariantUpper() : Contravariant[Something]
  covariant.getContravariantUpper() : Contravariant[Nothing]
  covariant.getContravariantUpper() : Contravariant[Unrelated]

  covariant.setUpper(any)
  covariant.setUpper(something)
  covariant.setUpper(nothing)
  covariant.setUpper(unrelated)

  covariant.setInvariantUpper(new Invariant[Any])
  covariant.setInvariantUpper(new Invariant[Something])
  //covariant.setInvariantUpper(new Invariant[Nothing])
  //covariant.setInvariantUpper(new Invariant[Unrelated])

  covariant.setCovariantUpper(new Covariant[Any])
  covariant.setCovariantUpper(new Covariant[Something])
  covariant.setCovariantUpper(new Covariant[Nothing])
  covariant.setCovariantUpper(new Covariant[Unrelated])

  covariant.setContravariant(new Contravariant[Any])
  covariant.setContravariant(new Contravariant[Something])
  //covariant.setContravariant(new Contravariant[Nothing])
  //covariant.setContravariant(new Contravariant[Unrelated])

  //new Covariant[Any]       : Covariant[Something]
  new Covariant[Something] : Covariant[Something]
  new Covariant[Nothing]   : Covariant[Something]
  //new Covariant[Unrelated] : Covariant[Something]
}
