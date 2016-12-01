object Streams extends App {
  def printSome[T](stream: Stream[T], count: Int): Unit = {
    stream.take(count) foreach { x => print(x + " ") }
    println()
  }

  val fibs: Stream[Int] = 0 #:: 1 #:: {(fibs zip fibs.tail) map { case (left, right) => left + right }}
  printSome(fibs, 10)

  def fiboStream(first: Int, second: Int): Stream[Int] = {
    val third = first + second
    //println("Calculating %d + %d = %d".format(first, second, third))
    Stream.cons(first, fiboStream(second, third))
  }
  val fibo = fiboStream(0, 1)
  printSome(fibo, 10)

  val fibo2 = Stream.iterate((0, 1)) { case (first, second) => (second, first + second) } map { case (left, right) => left }
  printSome(fibo2, 10)
}
