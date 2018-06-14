package transposed

import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait ColumnPrepender[C, M, RTag <: ListTag, MW] {
  def apply(c: C, m: M): MW
}

object ColumnPrepender {

  private def columnPrepend[
    C, CH, CT,
    M, MH, MT,
    RTag <: ListTag,
    MW, MWH, MWT,
  ](
    unconsM: Uncons[M, MH, MT],
    unconsC: Uncons[C, CH, CT],
    consMWH: Cons[CH, MH, MWH],
    prependColumnTail: ColumnPrepender[CT, MT, RTag, MWT],
    consMW: Cons[MWH, MWT, MW],
    toSingleColumnMatrix: ToSingleColumnMatrix[C, RTag, MW],
  )(c: C, m: M): MW =
    unconsM(m) map { case (mh, mt) =>
      val Some((ch, ct)) = unconsC(c)
      val mwh = consMWH(ch, mh)
      val mwt = prependColumnTail(ct, mt)
      consMW(mwh, mwt)
    } getOrElse {
      toSingleColumnMatrix(c)
    }

  implicit def seqColumnPrepender[
    S[X] <: S SeqOf X,
    A,
    MR,
    RTag <: ListTag,
    MRW,
  ](
    implicit
      unconsM: Uncons[S[MR], MR, S[MR]],
      unconsC: Uncons[S[A], A, S[A]],
      consMWH: Cons[A, MR, MRW],
      consMW: Cons[MRW, S[MRW], S[MRW]],
      toSingleColumnMatrix: ToSingleColumnMatrix[S[A], RTag, S[MRW]],
  ): ColumnPrepender[
    S[A],   // C
    S[MR],  // M
    RTag,   // RTag
    S[MRW], // MW
  ] =
    new ColumnPrepender[S[A], S[MR], RTag, S[MRW]] {
      def apply(c: S[A], m: S[MR]): S[MRW] =
        columnPrepend[
          S[A], A, S[A],       // C, CH, CT,
          S[MR], MR, S[MR],    // M, MH, MT,
          RTag,                // RTag,
          S[MRW], MRW, S[MRW], // MW, MWH, MWT
        ](
          unconsM = unconsM,
          unconsC = unconsC,
          consMWH = consMWH,
          prependColumnTail = this,
          consMW = consMW,
          toSingleColumnMatrix = toSingleColumnMatrix,
        )(c, m)
    }

  implicit def seqOfSeqsColumnPrepender[
    S1[X] <: S1 SeqOf X,
    S2[X] <: S2 SeqOf X,
    A,
  ](
    implicit
      consMWH: Cons[A, S2[A], S2[A]],
      consMW: Cons[S2[A], S1[S2[A]], S1[S2[A]]],
      toSingleColumnMatrix: ToSingleColumnMatrix[S1[A], SeqListTag[S2], S1[S2[A]]],
  ): ColumnPrepender[
    S1[A],          // C
    S1[S2[A]],      // M
    SeqListTag[S2], // RTag
    S1[S2[A]],      // MW
  ] =
    seqColumnPrepender[
      S1,             // S
      A,              // A
      S2[A],          // MR
      SeqListTag[S2], // RTag,
      S2[A],          // MRW
    ]

  implicit def hnilColumnPrepender[
    RTag <: ListTag,
  ](
    implicit
      unconsM: Uncons[HNil, Nothing, Nothing],
      toSingleColumnMatrix: ToSingleColumnMatrix[HNil, RTag, HNil],
  ): ColumnPrepender[
    HNil, // C
    HNil, // M
    RTag, // RTag,
    HNil, // MW
  ] =
    new ColumnPrepender[HNil, HNil, RTag, HNil] {
      def apply(c: HNil, m: HNil): HNil =
        columnPrepend[
          HNil, Nothing, Nothing, // C, CH, CT,
          HNil, Nothing, Nothing, // M, MH, MT,
          RTag,                   // RTag,
          HNil, Nothing, Nothing, // MW, MWH, MWT,
        ](
          unconsM = unconsM,
          unconsC = Uncons.dummy,
          consMWH = Cons.dummy,
          prependColumnTail = ColumnPrepender.dummy,
          consMW = Cons.dummy,
          toSingleColumnMatrix = toSingleColumnMatrix,
        )(c, m)
    }

  implicit def transposedHNilColumnPrepender[
    C, CH, CT,
    MW, MWH, MWT,
  ](
    implicit
      unconsM: Uncons[TransposedHNil, Nothing, Nothing],
      unconsC: Uncons[C, CH, CT],
      toSingleColumnMatrix: ToSingleColumnMatrix[C, HListListTag, MW],
      unconsMW: Uncons[MW, MWH, MWT],
  ): ColumnPrepender[
    C,              // C
    TransposedHNil, // M
    HListListTag,   // RTag
    MW,             // MW
  ] =
    new ColumnPrepender[C, TransposedHNil, HListListTag, MW] {
      def apply(c: C, m: TransposedHNil): MW =
        columnPrepend[
          C, CH, CT,                        // C, CH, CT,
          TransposedHNil, Nothing, Nothing, // M, MH, MT,
          HListListTag,                     // RTag,
          MW, MWH, MWT,                     // MW, MWH, MWT,
        ](
          unconsM = unconsM,
          unconsC = Uncons.dummy,
          consMWH = Cons.dummy,
          prependColumnTail = ColumnPrepender.dummy,
          consMW = Cons.dummy,
          toSingleColumnMatrix = toSingleColumnMatrix,
        )(c, m)
    }


  implicit def hconsOfSeqsColumnPrepender[
    S[X] <: S SeqOf X,
    CH, CT <: HList,
    MT <: HList,
  ](
    implicit
      unconsC: Uncons[CH :: CT, CH, CT],
      consMWH: Cons[CH, S[CH], S[CH]],
      prependColumnTail: ColumnPrepender[CT, MT, SeqListTag[S], MT],
      unconsM: Uncons[S[CH] :: MT, S[CH], MT],
      consMW: Cons[S[CH], MT, S[CH] :: MT],
  ): ColumnPrepender[
    CH :: CT,      // C
    S[CH] :: MT,   // M
    SeqListTag[S], // RTag,
    S[CH] :: MT,   // MW
  ] =
    new ColumnPrepender[CH :: CT, S[CH] :: MT, SeqListTag[S], S[CH] :: MT] {
      def apply(c: CH :: CT, m: S[CH] :: MT): S[CH] :: MT =
        columnPrepend[
          CH :: CT, CH, CT,       // C, CH, CT,
          S[CH] :: MT, S[CH], MT, // M, MH, MT,
          SeqListTag[S],          // RTag,
          S[CH] :: MT, S[CH], MT, // MW, MWH, MWT,
        ](
          unconsM = unconsM,
          unconsC = unconsC,
          consMWH = consMWH,
          prependColumnTail = prependColumnTail,
          consMW = consMW,
          toSingleColumnMatrix = ToSingleColumnMatrix.dummy,
        )(c, m)
    }

  implicit def hconsOfHListsColumnPrepender[
    CH, CT <: HList,
    MH <: HList, MT <: HList,
    MWT <: HList,
  ](
    implicit
      unconsM: Uncons[MH :: MT, MH, MT],
      unconsC: Uncons[CH :: CT, CH, CT],
      consMWH: Cons[CH, MH, CH :: MH],
      prependColumnTail: ColumnPrepender[CT, MT, HListListTag, MWT],
      consMW: Cons[CH :: MH, MWT, (CH :: MH) :: MWT],
  ): ColumnPrepender[
    CH :: CT,          // C
    MH :: MT,          // M
    HListListTag,      // RTag
    (CH :: MH) :: MWT, // MW
  ] =
    new ColumnPrepender[CH :: CT, MH :: MT, HListListTag, (CH :: MH) :: MWT] {
      def apply(c: CH :: CT, m: MH :: MT): (CH :: MH) :: MWT =
        columnPrepend[
          CH :: CT, CH, CT,                 // C, CH, CT,
          MH :: MT, MH, MT,                 // M, MH, MT,
          HListListTag,                     // RTag,
          (CH :: MH) :: MWT, CH :: MH, MWT, // MW, MWH, MWT,
        ](
          unconsM = unconsM,
          unconsC = unconsC,
          consMWH = consMWH,
          prependColumnTail = prependColumnTail,
          consMW = consMW,
          toSingleColumnMatrix = ToSingleColumnMatrix.dummy,
        )(c, m)
    }

  def dummy[
    C,
    RTag <: ListTag,
    MW,
  ]: ColumnPrepender[
    C,       // C
    Nothing, // M
    RTag,    // RTag
    MW,      // MW
  ] =
    (_: C, _: Nothing) => UnreachableCode_!!!
}
