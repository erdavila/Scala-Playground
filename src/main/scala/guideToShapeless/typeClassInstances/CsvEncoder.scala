package guideToShapeless.typeClassInstances

import shapeless.:+:
import shapeless.::
import shapeless.CNil
import shapeless.Coproduct
import shapeless.Generic
import shapeless.HList
import shapeless.HNil
import shapeless.Inl
import shapeless.Inr
import shapeless.Lazy

trait CsvEncoder[A] {
  def encode(value: A): List[String]
}

object CsvEncoder {
  def writeCsv[A](values: List[A])(implicit enc: CsvEncoder[A]): String =
    values.map(value => enc.encode(value).mkString(",")).mkString("\n")

  // "Summoner" method
  def apply[A](implicit enc: CsvEncoder[A]): CsvEncoder[A] =
    enc

  // "Constructor" method
  def createEncoder[A](func: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      def encode(value: A): List[String] =
        func(value)
    }

  implicit val booleanEncoder: CsvEncoder[Boolean] =
    createEncoder(b => if (b) List("yes") else List("no"))

  implicit val intEncoder: CsvEncoder[Int] =
    createEncoder(num => List(num.toString))

  implicit val doubleEncoder: CsvEncoder[Double] =
    createEncoder(num => List(num.toString))

  implicit val stringEncoder: CsvEncoder[String] =
    createEncoder(str => List(str))

  implicit def pairEncoder[A, B](
    implicit
      aEncoder: CsvEncoder[A],
      bEncoder: CsvEncoder[B]
  ): CsvEncoder[(A, B)] =
    createEncoder { pair =>
      val (a, b) = pair
      aEncoder.encode(a) ++ bEncoder.encode(b)
    }

  implicit val hnilEncoder: CsvEncoder[HNil] =
    createEncoder(hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit
      hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
      tEncoder: CsvEncoder[T],
  ): CsvEncoder[H :: T] =
    createEncoder {
      case h :: t =>
        hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
    implicit
      gen: Generic.Aux[A, R],
      rEncoder: Lazy[CsvEncoder[R]] // wrap in Lazy
  ): CsvEncoder[A] =
    createEncoder(a => rEncoder.value.encode(gen.to(a)))

  implicit val cnilEncoder: CsvEncoder[CNil] =
    createEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
      hEncoder: Lazy[CsvEncoder[H]], // wrap in Lazy
      tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :+: T] = createEncoder {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }
}
