package monads

sealed trait HList

object HList {
  trait Reverse[In <: HList, Out <: HList] {
    def apply(in: In): Out
  }

  object Reverse {
    implicit val hnilReverse: Reverse[HNil, HNil] =
      (_: HNil) => HNil

    implicit def hconsReverse
      [H, T <: HList, TR <: HList, R <: HList]
      (implicit
        revTail: Reverse[T, TR],
        append: Append[TR, H, R],
      )
    : Reverse[
      HCons[H, T],
      R,
    ] = (in: HCons[H, T]) => {
      val HCons(h, t) = in
      val tr = revTail(t)
      val r = append(tr, h)
      r
    }

    trait Append[In <: HList, Z, Out] {
      def apply(in: In, z: Z): Out
    }

    object Append {
      implicit def hnilAppend[Z]: Append[HNil, Z, HCons[Z, HNil]] =
        (in: HNil, z: Z) => HCons(z, in)

      implicit def hconsAppend
        [H, InT <: HList, Z, OutT <: HList]
        (implicit append: Append[InT, Z, OutT])
      : Append[HCons[H, InT], Z, HCons[H, OutT]] = (in: HCons[H, InT], z: Z) => {
        val HCons(h, inT) = in
        val outT = append(inT, z)
        HCons(h, outT)
      }
    }
  }
}

sealed trait HNil extends HList
object HNil extends HNil

case class HCons[H, T <: HList](head: H, tail: T) extends HList
