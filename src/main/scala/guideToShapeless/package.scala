package object guideToShapeless {

  def assertSameValueAndType[T](expected: T)(value: T): Unit =
    assert(value == expected)

  def sameTyped[T1, T2](v1: T1, v2: T2)(implicit ev: T1 =:= T2): Unit =
    ()

  def sameTypes[T1, T2]()(implicit ev: T1 =:= T2): Unit =
    ()
}
