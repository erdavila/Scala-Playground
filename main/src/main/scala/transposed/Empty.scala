package transposed

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait Empty[L] {
  def apply(): L
}

object Empty {

  implicit def emptySeq[
    S[X] <: S SeqOf X,
    A,
  ](
    implicit cbf: CanBuildFrom[_, A, S[A]]
  ): Empty[
    S[A], // L
  ] =
    () => {
      val builder = cbf()
      builder.result()
    }

  implicit def emptyHNil: Empty[
    HNil, // L
  ] =
    () => HNil

  implicit def emptyTransposedHNil: Empty[
    TransposedHNil // L
  ] =
    () => TransposedHNil

  implicit def emptyHConsOfSeqs[
    S[X] <: S SeqOf X,
    HA, T <: HList,
  ](
    implicit
      emptyHead: Empty[S[HA]],
      emptyTail: Empty[T],
  ): Empty[
    S[HA] :: T, // L
  ] =
    () => emptyHead() :: emptyTail()

  def dummy[
    L,
  ]: Empty[
    L, // L
  ] =
    () => UnreachableCode_!!!
}
