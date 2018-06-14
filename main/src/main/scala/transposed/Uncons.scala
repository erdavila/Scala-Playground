package transposed

import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait Uncons[L, H, T] {
  def apply(l: L): Option[(H, T)]
}

object Uncons {

  implicit def seqUncons[
    S[X] <: S SeqOf X,
    A,
  ]: Uncons[
    S[A], // L
    A,    // H
    S[A], // T
  ] =
    (l: S[A]) =>
      if (l.isEmpty) {
        None
      } else {
        Some((l.head, l.tail))
      }

  implicit def hnilUncons: Uncons[
    HNil,    // L
    Nothing, // H
    Nothing, // T
  ] =
    (_: HNil) => None

  implicit def transposedHNilUncons: Uncons[
    TransposedHNil, // L
    Nothing,        // H
    Nothing,        // T
  ] =
    (_: TransposedHNil) => None

  implicit def hconUncons[
    H, T <: HList,
  ]: Uncons[
    H :: T, // L
    H,      // H
    T,      // T
  ] =
    (l: H :: T) => Some((l.head, l.tail))

  def dummy[
    L, H, T
  ]: Uncons[
    L, // L
    H, // H
    T, // T
  ] =
    (_: L) => UnreachableCode_!!!
}
