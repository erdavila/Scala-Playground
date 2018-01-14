package variance2

import scala.reflect.runtime.universe.TypeTag

class Contravariant[-T] {
  //def get(): T = ???
  def getLower[U <: T](): U = ???

  //def getInvariant(): Invariant[T] = ???
  def getInvariantLower[U <: T](): Invariant[U] = ???

  //def getCovariant(): Covariant[T] = ???
  def getCovariantLower[U <: T](): Covariant[U] = ???

  def getContravariant(): Contravariant[T] = ???

  def set(value: T): Unit = ???

  //def setInvariant(value: Invariant[T]): Unit = ???
  def setInvariantLower[U <: T](value: Invariant[U]): Unit = ???

  def setCovariant(value: Covariant[T]): Unit = ???

  //def setContravariant(value: Contravariant[T]): Unit = ???
  def setContravariantLower[U <: T](value: Contravariant[U]): Unit = ???
}

object ContravariantTest extends Values {
  val contravariant = new Contravariant[Something]

  contravariant.getLower() : Any
  contravariant.getLower() : Something
  contravariant.getLower() : Nothing
  contravariant.getLower() : Unrelated

  //contravariant.getInvariantLower() : Invariant[Any]
  contravariant.getInvariantLower() : Invariant[Something]
  contravariant.getInvariantLower() : Invariant[Nothing]
  //contravariant.getInvariantLower() : Invariant[Unrelated]

  contravariant.getCovariantLower() : Covariant[Any]
  contravariant.getCovariantLower() : Covariant[Something]
  contravariant.getCovariantLower() : Covariant[Nothing]
  contravariant.getCovariantLower() : Covariant[Unrelated]

  //contravariant.getContravariant() : Contravariant[Any]
  contravariant.getContravariant() : Contravariant[Something]
  contravariant.getContravariant() : Contravariant[Nothing]
  //contravariant.getContravariant() : Contravariant[Unrelated]

  //contravariant.set(any)
  contravariant.set(something)
  contravariant.set(nothing)
  //contravariant.set(unrelated)

  //contravariant.setInvariantLower(new Invariant[Any])
  contravariant.setInvariantLower(new Invariant[Something])
  contravariant.setInvariantLower(new Invariant[Nothing])
  //contravariant.setInvariantLower(new Invariant[Unrelated])

  //contravariant.setCovariant(new Covariant[Any])
  contravariant.setCovariant(new Covariant[Something])
  contravariant.setCovariant(new Covariant[Nothing])
  //contravariant.setCovariant(new Covariant[Unrelated])

  contravariant.setContravariantLower(new Contravariant[Any])
  contravariant.setContravariantLower(new Contravariant[Something])
  contravariant.setContravariantLower(new Contravariant[Nothing])
  contravariant.setContravariantLower(new Contravariant[Unrelated])

  new Contravariant[Any]       : Contravariant[Something]
  new Contravariant[Something] : Contravariant[Something]
  //new Contravariant[Nothing]   : Contravariant[Something]
  //new Contravariant[Unrelated] : Contravariant[Something]
}
