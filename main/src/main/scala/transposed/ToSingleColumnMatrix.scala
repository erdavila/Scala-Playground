package transposed

import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait ToSingleColumnMatrix[L, RTag <: ListTag, M] {
  def apply(l: L): M
}

object ToSingleColumnMatrix {

  private def toSingleColumnMatrix[
    L, LH, LT,
    RTag <: ListTag,
    M, MH, MT,
  ](
    unconsL: Uncons[L, LH, LT],
    singleMH: Single[LH, RTag, MH],
    tailToSingleColumnMatrix: ToSingleColumnMatrix[LT, RTag, MT],
    consM: Cons[MH, MT, M],
    emptyM: Empty[M],
  )(l: L): M =
    unconsL(l) map { case (lh, lt) =>
      val mh = singleMH(lh)
      val mt = tailToSingleColumnMatrix(lt)
      consM(mh, mt)
    } getOrElse {
      emptyM()
    }

  implicit def toSingleColumnSeqMatrix[
    S[X] <: S SeqOf X,
    A,
    RTag <: ListTag,
    MR,
  ](
    implicit
      unconsL: Uncons[S[A], A, S[A]],
      singleMH: Single[A, RTag, MR],
      consM: Cons[MR, S[MR], S[MR]],
      emptyM: Empty[S[MR]],
  ): ToSingleColumnMatrix[
    S[A],
    RTag,
    S[MR],
  ] =
    new ToSingleColumnMatrix[S[A], RTag, S[MR]] {
      def apply(l: S[A]): S[MR] =
        toSingleColumnMatrix[
          S[A], A, S[A],    // L, LH, LT,
          RTag,             // RTag,
          S[MR], MR, S[MR], // M, MH, MT,
        ](
          unconsL = unconsL,
          singleMH = singleMH,
          tailToSingleColumnMatrix = this,
          consM = consM,
          emptyM = emptyM,
        )(l)
    }

  implicit def toSingleColumnHNilMatrix[
    RTag <: ListTag,
  ](
    implicit
      unconsL: Uncons[HNil, Nothing, Nothing],
      emptyM: Empty[HNil],
  ): ToSingleColumnMatrix[
    HNil, // L
    RTag, // RTag
    HNil, // M
  ] =
    new ToSingleColumnMatrix[HNil, RTag, HNil] {
      def apply(l: HNil): HNil =
        toSingleColumnMatrix[
          HNil, Nothing, Nothing, // L, LH, LT,
          RTag,                   // RTag,
          HNil, Nothing, Nothing, // M, MH, MT,
        ](
          unconsL = unconsL,
          singleMH = Single.dummy,
          tailToSingleColumnMatrix = ToSingleColumnMatrix.dummy,
          consM = Cons.dummy,
          emptyM = emptyM,
        )(l)
    }

  implicit def toSingleColumnHConsMatrix[
    LH, LT <: HList,
    RTag <: ListTag,
    MH, MT <: HList,
  ](
    implicit
      unconsL: Uncons[LH :: LT, LH, LT],
      singleMH: Single[LH, RTag, MH],
      tailToSingleColumnMatrix: ToSingleColumnMatrix[LT, RTag, MT],
      consM: Cons[MH, MT, MH :: MT],
  ): ToSingleColumnMatrix[
    LH :: LT, // L
    RTag,     // RTag
    MH :: MT, // M
  ] =
    new ToSingleColumnMatrix[LH :: LT, RTag, MH :: MT] {
      def apply(l: LH :: LT): MH :: MT =
        toSingleColumnMatrix[
          LH :: LT, LH, LT, // L, LH, LT,
          RTag,             // RTag,
          MH :: MT, MH, MT, // M, MH, MT,
        ](
          unconsL = unconsL,
          singleMH = singleMH,
          tailToSingleColumnMatrix = tailToSingleColumnMatrix,
          consM = consM,
          emptyM = Empty.dummy,
        )(l)
    }

  def dummy[
    L,
    RTag <: ListTag,
    M,
  ]: ToSingleColumnMatrix[
    L,    // L
    RTag, // RTag
    M,    // M
  ] =
    (_: L) => UnreachableCode_!!!
}
