package transposition

import scala.collection.SeqLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import shapeless.{::, HList}

trait Cons[H, T, Tag <: SeqTag, S] {
  def apply(h: H, t: T): S
}

object Cons {

  implicit def scalaSeqLikeCons[
    A,
    S[X] <: SeqLike[X, S[X]],
  ](
    implicit cbf: CanBuildFrom[S[A], A, S[A]]
  ): HomoSeqCons[
    A, // H
    S[A], // T
    HomoSeqTag[S], // Tag
  ] =
    (h: A, t: S[A]) => {
      val builder = cbf()
      builder += h
      builder ++= t
      builder.result()
    }

  implicit def hlistCons[
    H, T <: HList
  ]: Cons[
    H, // H
    T, // T
    HeteroSeqTag[HList], // Tag
    H :: T, // S
  ] =
    (h: H, t: T) => h :: t

  private[transposition] def dummy[
    H, T,
    Tag <: SeqTag,
    S,
  ]: Cons[
    H, // H
    T, // T
    Tag, // Tag
    S, // S
  ] =
    (h: H, t: T) => UnreachableCode_!!!
}
