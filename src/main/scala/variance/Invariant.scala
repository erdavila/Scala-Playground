package variance

class Invariant[T] {
  def expectsExact(a: T): Unit = ???
  def expectsUpper[U >: T](a: U): Unit = ???
  def expectsLower[U <: T](a: U): Unit = ???

  def returnsExact(): T = ???
  def returnsUpper[U >: T](): U = ???
  def returnsLower[U <: T](): U = ???
}

object Invariant extends Base[Invariant] {
  //expectsOfMiddle(ofTop)  // type mismatch;  found   : variance.Invariant[variance.Top]  required: variance.Invariant[variance.Middle] Note: variance.Top >: variance.Middle, but class Invariant is invariant in type T. You may wish to define T as -T instead. (SLS 4.5)
  expectsOfMiddle(ofMiddle)
  //expectsOfMiddle(ofBottom)  // type mismatch;  found   : variance.Invariant[variance.Bottom]  required: variance.Invariant[variance.Middle] Note: variance.Bottom <: variance.Middle, but class Invariant is invariant in type T. You may wish to define T as +T instead. (SLS 4.5)

  //expectsOfTop(ofMiddle)  // type mismatch;  found   : variance.Invariant[variance.Middle]  required: variance.Invariant[variance.Top] Note: variance.Middle <: variance.Top, but class Invariant is invariant in type T. You may wish to define T as +T instead. (SLS 4.5)
  expectsOfMiddle(ofMiddle)
  //expectsOfBottom(ofMiddle)  // type mismatch;  found   : variance.Invariant[variance.Middle]  required: variance.Invariant[variance.Bottom] Note: variance.Middle >: variance.Bottom, but class Invariant is invariant in type T. You may wish to define T as -T instead. (SLS 4.5)

  //ofMiddle.expectsExact(top)  // type mismatch;  found   : variance.Top  required: variance.Middle
  ofMiddle.expectsExact(middle)
  ofMiddle.expectsExact(bottom)

  ofMiddle.expectsUpper(top)
  ofMiddle.expectsUpper(middle)
  ofMiddle.expectsUpper(bottom)

  //ofMiddle.expectsLower(top)  // inferred type arguments [variance.Top] do not conform to method expectsLower's type parameter bounds [U <: variance.Middle]
  ofMiddle.expectsLower(middle)
  ofMiddle.expectsLower(bottom)

  expectsTop(ofMiddle.returnsExact())
  expectsMiddle(ofMiddle.returnsExact())
  //expectsBottom(ofMiddle.returnsExact())  // type mismatch;  found   : variance.Middle  required: variance.Bottom

  expectsTop(ofMiddle.returnsUpper())
  expectsMiddle(ofMiddle.returnsUpper())
  //expectsBottom(ofMiddle.returnsUpper())  // type mismatch;  found   : variance.Middle  required: variance.Bottom

  expectsTop(ofMiddle.returnsLower())
  expectsMiddle(ofMiddle.returnsLower())
  expectsBottom(ofMiddle.returnsLower())
}
