package variance

class Contravariant[-T] {
  def expectsExact(a: T): Unit = ???
  //def expectsUpper[U >: T](a: U): Unit = ???  // contravariant type T occurs in covariant position in type  >: T of type U
  def expectsLower[U <: T](a: U): Unit = ???

  //def returnsExact(): T = ???  // contravariant type T occurs in covariant position in type ()T of method returnsExact
  //def returnsUpper[U >: T](): U = ???  // contravariant type T occurs in covariant position in type  >: T of type U
  def returnsLower[U <: T](): U = ???
}

object Contravariant extends Base[Contravariant] {
  expectsOfMiddle(ofTop)
  expectsOfMiddle(ofMiddle)
  //expectsOfMiddle(ofBottom)  // type mismatch;  found   : variance.Contravariant[variance.Bottom]  required: variance.Contravariant[variance.Middle]

  //expectsOfTop(ofMiddle)  // type mismatch;  found   : variance.Contravariant[variance.Middle]  required: variance.Contravariant[variance.Top]
  expectsOfMiddle(ofMiddle)
  expectsOfBottom(ofMiddle)

  //ofMiddle.expectsExact(top)  // type mismatch;  found   : variance.Top  required: variance.Middle
  ofMiddle.expectsExact(middle)
  ofMiddle.expectsExact(bottom)

  /*
  ofMiddle.expectsUpper(top)
  ofMiddle.expectsUpper(middle)
  ofMiddle.expectsUpper(bottom)
  */

  //ofMiddle.expectsLower(top)  // inferred type arguments [variance.Top] do not conform to method expectsLower's type parameter bounds [U <: variance.Middle]
  ofMiddle.expectsLower(middle)
  ofMiddle.expectsLower(bottom)

  /*
  expectsTop(ofMiddle.returnsExact())
  expectsMiddle(ofMiddle.returnsExact())
  expectsBottom(ofMiddle.returnsExact())
  */

  /*
  expectsTop(ofMiddle.returnsUpper())
  expectsMiddle(ofMiddle.returnsUpper())
  expectsBottom(ofMiddle.returnsUpper())
  */

  expectsTop(ofMiddle.returnsLower())
  expectsMiddle(ofMiddle.returnsLower())
  expectsBottom(ofMiddle.returnsLower())

  class Producer[-T] {
    def produce[U <: T](): U = ???
  }
  val `producer of Middle (and Top)` : Producer[Middle] = ???
  //val `producer of Top`: Producer[Top] = `producer of Middle (and Top)`  // type mismatch;  found   : variance.Contravariant.Producer[variance.Middle]  required: variance.Contravariant.Producer[variance.Top]
  val `producer of Bottom (and Top and Middle)` : Producer[Bottom] = `producer of Middle (and Top)`

  class Consumer[-T] {
    def consume[T](value: T): Unit = ???
  }
  val `consumer of Middle (and Bottom)`: Consumer[Middle] = ???
  //val `consumer of Top (and Middle and Bottom)`: Consumer[Top] = `consumer of Middle (and Bottom)`  // type mismatch;  found   : variance.Contravariant.Consumer[variance.Middle]  required: variance.Contravariant.Consumer[variance.Top]
  val `consumer of Bottom`: Consumer[Bottom] = `consumer of Middle (and Bottom)`
}
