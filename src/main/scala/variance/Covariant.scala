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
}
