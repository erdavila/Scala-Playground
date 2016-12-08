package variance

class Covariant[+T] {
  //def expectsExact(a: T): Unit = ???  // covariant type T occurs in contravariant position in type T of value a
  def expectsUpper[U >: T](a: U): Unit = ???
  //def expectsLower[U <: T](a: U): Unit = ???  // covariant type T occurs in contravariant position in type  <: T of type U

  def returnsExact(): T = ???
  def returnsUpper[U >: T](): U = ???
  //def returnsLower[U <: T](): U = ???  // covariant type T occurs in contravariant position in type  <: T of type U
}

object Covariant extends Base[Covariant] {
  //expectsOfMiddle(ofTop)  // type mismatch;  found   : variance.Covariant[variance.Top]  required: variance.Covariant[variance.Middle]
  expectsOfMiddle(ofMiddle)
  expectsOfMiddle(ofBottom)

  expectsOfTop(ofMiddle)
  expectsOfMiddle(ofMiddle)
  //expectsOfBottom(ofMiddle)  // type mismatch;  found   : variance.Covariant[variance.Middle]  required: variance.Covariant[variance.Bottom]

  /*
  ofMiddle.expectsExact(top)
  ofMiddle.expectsExact(middle)
  ofMiddle.expectsExact(bottom)
  */

  ofMiddle.expectsUpper(top)
  ofMiddle.expectsUpper(middle)
  ofMiddle.expectsUpper(bottom)

  /*
  ofMiddle.expectsLower(top)
  ofMiddle.expectsLower(middle)
  ofMiddle.expectsLower(bottom)
  */

  expectsTop(ofMiddle.returnsExact())
  expectsMiddle(ofMiddle.returnsExact())
  //expectsBottom(ofMiddle.returnsExact())  // type mismatch;  found   : variance.Middle  required: variance.Bottom

  expectsTop(ofMiddle.returnsUpper())
  expectsMiddle(ofMiddle.returnsUpper())
  //expectsBottom(ofMiddle.returnsUpper())  // type mismatch;  found   : variance.Middle  required: variance.Bottom

  /*
  expectsTop(ofMiddle.returnsLower())
  expectsMiddle(ofMiddle.returnsLower())
  expectsBottom(ofMiddle.returnsLower())
  */

  class Producer[+T] {
    def produce(): T = ???
  }
  val `producer of Middle (and Bottom)` : Producer[Middle] = ???
  val `producer of Top (and Middle and Bottom)`: Producer[Top] = `producer of Middle (and Bottom)`
  //val `producer of Bottom` : Producer[Bottom] = `producer of Middle (and Bottom)`  // type mismatch;  found   : variance.Covariant.Producer[variance.Middle]  required: variance.Covariant.Producer[variance.Bottom]

  class Consumer[+T] {
    def consume[U >: T](value: U): Unit = ???
  }
  val `consumer of Middle (and Top)`: Consumer[Middle] = ???
  val `consumer of Top`: Consumer[Top] = `consumer of Middle (and Top)`
  //val `consumer of Bottom (and Top and Middle)`: Consumer[Bottom] = `consumer of Middle (and Top)`  //  type mismatch;  found   : variance.Covariant.Consumer[variance.Middle]  required: variance.Covariant.Consumer[variance.Bottom]
}
