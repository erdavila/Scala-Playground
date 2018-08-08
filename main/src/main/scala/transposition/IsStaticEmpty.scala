package transposition

import shapeless.{HList, HNil}

final class IsStaticEmpty[S, Tag <: SeqTag]

object IsStaticEmpty {

  implicit val hnilIsStaticEmpty: IsStaticEmpty[
    HNil,
    HeteroSeqTag[HList],
  ] =
    new IsStaticEmpty[HNil, HeteroSeqTag[HList]]
}
