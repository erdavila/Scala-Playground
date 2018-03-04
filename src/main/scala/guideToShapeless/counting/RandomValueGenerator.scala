package guideToShapeless.counting

import shapeless._
import shapeless.ops.coproduct
import shapeless.ops.nat.ToInt

private trait Random[A] {
  def get: A
}

private object Random {

  def get[A](implicit r: Random[A]): A = r.get

  // Instance constructor
  def createRandom[A](func: => A): Random[A] =
    new Random[A] {
      def get = func
    }

  // Random numbers from 0 to 9
  implicit val intRandom: Random[Int] =
    createRandom(scala.util.Random.nextInt(10))

  // Random characters from 'A' to 'Z'
  implicit val charRandom: Random[Char] =
    createRandom(('A' + scala.util.Random.nextInt(26)).toChar)

  // Random booleans
  implicit val booleanRandom: Random[Boolean] =
    createRandom(scala.util.Random.nextBoolean())

  implicit def genericRandom[A, R](
    implicit
      gen: Generic.Aux[A, R],
      random: Lazy[Random[R]]
  ): Random[A] =
    createRandom(gen.from(random.value.get))

  implicit def hnilRandom: Random[HNil] =
    createRandom(HNil)

  implicit def hlistRandom[H, T <: HList](
    implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T]
  ): Random[H :: T] =
    createRandom(hRandom.value.get :: tRandom.get)

  implicit val cnilRandom: Random[CNil] =
    createRandom(throw new Exception("Inconceivable!"))

  implicit def coproductRandom[H, T <: Coproduct, L <: Nat](
    implicit
      hRandom: Lazy[Random[H]],
      tRandom: Random[T],
      tLength: coproduct.Length.Aux[T, L],
      tLengthAsInt: ToInt[L]
  ): Random[H :+: T] =
    createRandom {
      val length = 1 + tLengthAsInt()
      val chooseH = scala.util.Random.nextDouble() < (1.0 / length)
      if (chooseH) Inl(hRandom.value.get) else Inr(tRandom.get)
    }
}

object RandomValueGenerator {

  private case class Cell(col: Char, row: Int)

  private sealed trait Light
  private case object Red extends Light
  private case object Amber extends Light
  private case object Green extends Light

  def main(args: Array[String]): Unit = {
    for (i <- 1 to 3) println(Random.get[Int])
    for (i <- 1 to 3) println(Random.get[Char])
    for (i <- 1 to 5) println(Random.get[Cell])
    for (i <- 1 to 5) println(Random.get[Light])
  }
}
