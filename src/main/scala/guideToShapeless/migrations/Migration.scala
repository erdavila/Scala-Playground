package guideToShapeless.migrations

import shapeless.::
import shapeless.HList
import shapeless.HNil
import shapeless.LabelledGeneric
import shapeless.Lazy
import shapeless.labelled.FieldType
import shapeless.labelled.field
import shapeless.ops.hlist

trait Migration[A, B] {
  def apply(a: A): B
}

object Migration {

  object Implicits {
    implicit val hnilMonoid: Monoid[HNil] =
      Monoid.createMonoid[HNil](HNil)((x, y) => HNil)

    implicit def emptyHList[K <: Symbol, H, T <: HList](
      implicit
        hMonoid: Lazy[Monoid[H]],
        tMonoid: Monoid[T]
    ): Monoid[FieldType[K, H] :: T] =
      Monoid.createMonoid(field[K](hMonoid.value.empty) :: tMonoid.empty) { (x, y) =>
        val head = field[K](hMonoid.value.combine(x.head, y.head))
        val tail = tMonoid.combine(x.tail, y.tail)
        head :: tail
      }
  }

  implicit def genericMigration[
    A, B, ARepr <: HList, BRepr <: HList,
    Common <: HList, Added <: HList, Unaligned <: HList
  ](
    implicit
      aGen:    LabelledGeneric.Aux[A, ARepr],
      bGen:    LabelledGeneric.Aux[B, BRepr],
      inter:   hlist.Intersection.Aux[ARepr, BRepr, Common],
      diff:    hlist.Diff.Aux[BRepr, Common, Added],
      monoid:  Monoid[Added],
      prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
      align:   hlist.Align[Unaligned, BRepr]
  ): Migration[A, B] =
    new Migration[A, B] {
      def apply(a: A): B =
        bGen.from(
          align(
            prepend(
              monoid.empty, inter(aGen.to(a))
            )
          )
        )
    }

  implicit class Ops[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }
}
