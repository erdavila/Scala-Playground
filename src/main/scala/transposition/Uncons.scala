package transposition

import scala.collection.SeqLike
import scala.language.higherKinds
import shapeless.{::, HList}

trait Uncons[S, Tag <: SeqTag, H, T] {
  def apply(s: S): Option[(H, T)]
}

object Uncons {

  implicit def scalaSeqLikeUncons[
    S[X] <: SeqLike[X, S[X]],
    A,
  ]: HomoSeqUncons[
    S[A], // S
    HomoSeqTag[S], // Tag
    A, // A
  ] =
    (s: S[A]) =>
      if (s.isEmpty) {
        None
      } else {
        Some((s.head, s.tail))
      }

  implicit def hlistUncons[
    H, T <: HList
  ]: Uncons[
    H :: T, // S
    HeteroSeqTag[HList], // Tag
    H, // H
    T, // T
  ] =
    (s: H :: T) => Some((s.head, s.tail))

  private[transposition] def emptySeqUncons[
    S,
    Tag <: SeqTag,
  ]: Uncons[
    S, // S
    Tag, // Tag
    Nothing, // H
    Nothing, // T
  ] =
    (_: S) => None

  private[transposition] def dummy[
    S,
    Tag <: SeqTag,
    H, T,
  ]: Uncons[
    S, // S
    Tag, // Tag
    H, // H
    T, // T
  ] =
    (s: S) => UnreachableCode_!!!
}
