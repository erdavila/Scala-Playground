package mapgeneric

import shapeless._
import shapeless.::
import shapeless.labelled.FieldType
import shapeless.labelled.field
import shapeless.syntax.typeable._

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
      hValueFrom: ValueFrom[V],
      tMapToRepr: MapToRepr[T]
  ): MapToRepr[FieldType[K, V] :: T] =
    create[FieldType[K, V] :: T] { map =>
      val key = hKeyWitness.value.name
      val valueOption = map.get(key)
      for {
        value <- hValueFrom(valueOption)
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

trait MaybeFromMap[A] {
  def apply(map: Map[String, Any]): Option[A]
}

trait LowPriorityMaybeFromMap {
  implicit def withoutFromMap[A]: MaybeFromMap[A] =
    create[A] { _ => None }

  protected def create[A](f: Map[String, Any] => Option[A]): MaybeFromMap[A] =
    new MaybeFromMap[A] {
      def apply(map: Map[String, Any]): Option[A] = f(map)
    }
}

object MaybeFromMap extends LowPriorityMaybeFromMap {
  implicit def withFromMap[A](implicit fromMap: FromMap[A]): MaybeFromMap[A] =
    create[A] { fromMap(_) }
}

trait ValueFrom[V] {
  def apply(option: Option[Any]): Option[V]
}

trait LowPriorityValueFrom {
  implicit def valueFrom[V](
    implicit
      typeable: Typeable[V],
      maybeFromMap: MaybeFromMap[V]
  ): ValueFrom[V] =
    create[V] { option =>
      option flatMap { value =>
        val cast = typeable.cast(value)
        lazy val fromMap = value.cast[Map[String, Any]].flatMap { maybeFromMap(_) }
        cast orElse fromMap
      }
    }

  protected def create[V](f: Option[Any] => Option[V]): ValueFrom[V] =
    new ValueFrom[V] {
      def apply(option: Option[Any]): Option[V] = f(option)
    }
}

object ValueFrom extends LowPriorityValueFrom {
  implicit def valueFromOption[V](implicit valueFrom: ValueFrom[V]): ValueFrom[Option[V]] =
    create[Option[V]] { option =>
      option match {
        case Some(_) => valueFrom(option) map { Some(_) }
        case None => Some(None)
      }
    }
}
