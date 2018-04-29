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
      hValueOf: ValueOf[V],
      tReprToMap: ReprToMap[T]
  ): ReprToMap[FieldType[K, V] :: T] =
    create[FieldType[K, V] :: T] { repr =>
      val h = hValueOf(repr.head) map { hKeyWitness.value.name -> _ }
      val t = tReprToMap(repr.tail)
      h ++: t
    }

  private def create[R <: HList](f: R => Map[String, Any]): ReprToMap[R] =
    new ReprToMap[R] {
      def apply(repr: R): Map[String, Any] = f(repr)
    }
}

trait ValueOf[V] {
  type Out
  def apply(field: V): Option[Out]
}

trait LowPriorityValueOf {
  protected type Aux[V, O] = ValueOf[V] { type Out = O }

  implicit def valueOf[V]: Aux[V, V] =
    create[V, V] { Some(_) }

  protected def create[V, O](f: V => Option[O]): Aux[V, O] =
    new ValueOf[V] {
      type Out = O
      def apply(field: V): Option[Out] = f(field)
    }
}

object ValueOf extends LowPriorityValueOf {
  implicit def valueOfOption[V, O](implicit valueOf: Aux[V, O]): Aux[Option[V], O] =
    create[Option[V], O] { option =>
      option flatMap { valueOf(_) }
    }

  implicit def valueOfCaseClass[CC](implicit ToMap: ToMap[CC]): Aux[CC, Map[String, Any]] =
    create[CC, Map[String, Any]] { cc =>
      Some(ToMap(cc))
    }
}
