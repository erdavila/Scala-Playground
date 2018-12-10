package transposition

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds
import shapeless.{HList, HNil}

trait Empty[Tag <: SeqTag, S] {
  def apply(): S
}

object Empty {

  implicit def scalaSeqEmpty[
    S[_] <: Seq[_],
    A,
  ](
    implicit cbf: CanBuildFrom[S[A], A, S[A]]
  ): Empty[
    HomoSeqTag[S], // Tag
    S[A], // S
  ] =
    () => {
      val builder = cbf()
      builder.result()
    }

  implicit val hnilEmpty: Empty[
    HeteroSeqTag[HList], // Tag
    HNil, // S
  ] =
    () => HNil

  implicit def heteroSeqOfHomoSeqsEmpty[
    S,
    STag <: HeteroSeqTag[_],
    HTag <: SeqTag,
    H, T,
  ](
    implicit
      consS: Cons[H, T, STag, S],
      emptyH: Empty[HTag, H],
      emptyT: Empty[STag, T],
  ): Empty[
    STag, // Tag
    S, // S
  ] =
    () => {
      val h = emptyH()
      val t = emptyT()
      consS(h, t)
    }

  private[transposition] def dummy[
    Tag <: SeqTag,
    S,
  ]: Empty[
    Tag, // Tag
    S, // S
  ] =
    () => UnreachableCode_!!!
}
