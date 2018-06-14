package transposed

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait Cons[H, T, L] {
  def apply(h: H, t: T): L
}

object Cons {

  implicit def seqCons[
    S[X] <: S SeqOf X,
    A
  ](
    implicit cbf: CanBuildFrom[_, A, S[A]]
  ): Cons[
    A,    // H
    S[A], // T
    S[A], // L
  ] =
    (h: A, t: S[A]) => {
      val builder = cbf()
      builder += h
      builder ++= t
      builder.result()
    }

  implicit def hconsCons[
    H, T <: HList,
  ]: Cons[
    H,      // H
    T,      // T
    H :: T, // L
  ] =
    (h: H, t: T) => h :: t

  def dummy[
    H, T, L,
  ]: Cons[
    H, // H
    T, // T
    L, // L
  ] =
    (_: H, _: T) => UnreachableCode_!!!
}
