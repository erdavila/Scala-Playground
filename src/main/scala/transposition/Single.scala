package transposition

import scala.language.higherKinds

sealed trait Single[A, Tag <: SeqTag, S] {
  def apply(a: A): S
}

object Single {

  implicit def homoSeqSingle[
    A,
    Tag <: SeqTag,
    S,
  ](
    implicit
      cons: HomoSeqCons[A, S, Tag],
      empty: Empty[Tag, S],
  ): Single[
    A, // A <-
    Tag, // Tag <-
    S, // S ->
  ] =
    new Single[A, Tag, S] {
      override def apply(a: A): S =
        cons(a, empty())
    }

  implicit def heteroSeqSingle[
    A,
    Tag <: HeteroSeqTag[_],
    E, S,
  ](
    implicit
      empty: Empty[Tag, E],
      cons: Cons[A, E, Tag, S],
  ): Single[
    A, // A <-
    Tag, // Tag <-
    S, // S ->
  ] =
    new Single[A, Tag, S] {
      override def apply(a: A): S =
        cons(a, empty())
    }

  private[transposition] def dummy[
    A,
  ]: Single[
    A, // A
    Nothing, // Tag
    Nothing, // S
  ] =
    new Single[A, Nothing, Nothing] {
      override def apply(a: A): Nothing =
        UnreachableCode_!!!
    }
}
