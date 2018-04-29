package mapgeneric

import shapeless._
import shapeless.::
import shapeless.labelled.FieldType
import shapeless.labelled.field

trait FromMap[A] {
  def apply(map: Map[String, Any]): Option[A]
}

object FromMap {
  implicit def instance[A, R <: HList](
    implicit
      gen: LabelledGeneric.Aux[A, R],
      mapToRepr: MapToRepr[R]
  ): FromMap[A] =
    new FromMap[A] {
      def apply(map: Map[String, Any]): Option[A] =
        mapToRepr(map) map { gen.from }
    }
}

trait MapToRepr[R <: HList] {
  def apply(map: Map[String, Any]): Option[R]
}

object MapToRepr {
  implicit val hnilMapToRepr: MapToRepr[HNil] =
    create[HNil](_ => Some(HNil))

  implicit def hconsMapToRepr[K <: Symbol, V, T <: HList](
    implicit
      hKeyWitness: Witness.Aux[K],
      hTypeable: Typeable[V],
      tMapToRepr: MapToRepr[T]
  ): MapToRepr[FieldType[K, V] :: T] =
    create[FieldType[K, V] :: T] { map =>
      val key = hKeyWitness.value.name
      for {
        valueAny <- map.get(key)
        value <- hTypeable.cast(valueAny)
        t <- tMapToRepr(map)
      } yield {
        val h = field[K](value)
        h :: t
      }
    }

  private def create[R <: HList](f: Map[String, Any] => Option[R]): MapToRepr[R] =
    new MapToRepr[R] {
      def apply(map: Map[String, Any]): Option[R] = f(map)
    }
}
