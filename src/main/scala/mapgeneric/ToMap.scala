package mapgeneric

import shapeless._
import shapeless.::
import shapeless.labelled.FieldType

trait ToMap[A] {
  def apply(a: A): Map[String, Any]
}

object ToMap {
  implicit def instance[A, R <: HList](
    implicit
      gen: LabelledGeneric.Aux[A, R],
      reprToMap: ReprToMap[R],
  ): ToMap[A] =
    new ToMap[A] {
      def apply(a: A): Map[String, Any] = {
        val repr = gen.to(a)
        reprToMap(repr)
      }
    }
}

trait ReprToMap[R <: HList] {
  def apply(repr: R): Map[String, Any]
}

object ReprToMap {
  implicit val hnilReprToMap: ReprToMap[HNil] =
    create[HNil](_ => Map.empty)

  implicit def hconsReprToMap[K <: Symbol, V, T <: HList](
    implicit
      hKeyWitness: Witness.Aux[K],
      tReprToMap: ReprToMap[T]
  ): ReprToMap[FieldType[K, V] :: T] =
    create[FieldType[K, V] :: T] { repr =>
      val h = Map(hKeyWitness.value.name -> repr.head)
      val t = tReprToMap(repr.tail)
      h ++ t
    }

  private def create[R <: HList](f: R => Map[String, Any]): ReprToMap[R] =
    new ReprToMap[R] {
      def apply(repr: R): Map[String, Any] = f(repr)
    }
}
