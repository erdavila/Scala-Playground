package guideToShapeless.lemmaPatern

import shapeless.ops.hlist
import shapeless.HList
import shapeless.Generic

trait Penultimate[L] {
  type Out
  def apply(l: L): Out
}

object Penultimate {
  type Aux[L, O] = Penultimate[L] { type Out = O }

  def apply[L](implicit p: Penultimate[L]): Aux[L, p.Out] = p

  implicit def hlistPenultimate[L <: HList, M <: HList, O](
    implicit
      init: hlist.Init.Aux[L, M],
      last: hlist.Last.Aux[M, O]
  ): Aux[L, O] =
    new Penultimate[L] {
      type Out = O
      def apply(l: L): O =
        //l.init.last
        last.apply(init.apply(l))
    }

  implicit def genericPenultimate[A, R, O](
    implicit
      generic: Generic.Aux[A, R],
      penultimate: Aux[R, O]
  ): Aux[A, O] =
    new Penultimate[A] {
      type Out = O
      def apply(a: A): O =
        penultimate.apply(generic.to(a))
    }

  implicit class Ops[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out =
      inst.apply(a)
  }
}
