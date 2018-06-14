package transposed

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait Single[A, Tag <: ListTag, L] {
  def apply(a: A): L
}

object Single {

  implicit def seqSingle[
    S[X] <: S SeqOf X,
    A,
  ](
    implicit cbf: CanBuildFrom[_, A, S[A]]
  ): Single[
    A,             // A
    SeqListTag[S], // Tag
    S[A],          // L
  ] =
    (a: A) => {
      val builder = cbf()
      builder += a
      builder.result()
    }

  implicit def singleElementHListSingle[
    H,
  ]: Single[
    H   ,         // A
    HListListTag, // Tag
    H :: HNil,    // L
  ] =
    (a: H) => a :: HNil

  implicit def multiElementHListSingle[
    H, T <: HList,
  ]: Single[
    H,            // A
    HListListTag, // Tag
    H :: T,       // L
  ] =
    (_: H) => UnreachableCode_!!!

  def dummy[
    RTag <: ListTag,
  ]: Single[
    Nothing, // A
    RTag,    // Tag
    Nothing, // L
  ] =
    (_: Nothing) => UnreachableCode_!!!
}
