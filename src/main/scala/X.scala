object X {
  def f(): Unit = {
    val thisIsTheVal = 0
    Y.m { () => thisIsTheVal }
  }
}
