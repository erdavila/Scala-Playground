package transposition

trait ColumnPrepender[C, M, CTag <: SeqTag, RTag <: SeqTag, MW] {
  def apply(c: C, m: M): MW
}

object ColumnPrepender {

  private def columnPrepend[
    C, CH, CT,
    M, MH, MT,
    CTag <: SeqTag, // for C, CT, M, MT, MW, MWT
    RTag <: SeqTag, // for MH, MWH
    MW, MWH, MWT,
  ](
    unconsM: Uncons[M, CTag, MH, MT],
    unconsC: Uncons[C, CTag, CH, CT],
    consMWH: Cons[CH, MH, RTag, MWH],
    prependColumnTail: ColumnPrepender[CT, MT, CTag, RTag, MWT],
    consMW: Cons[MWH, MWT, CTag, MW],
    toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  )(c: C, m: M): MW =
    unconsM(m) map { case (mh, mt) =>
      val Some((ch, ct)) = unconsC(c)
      val mwh = consMWH(ch, mh)
      val mwt = prependColumnTail(ct, mt)
      consMW(mwh, mwt)
    } getOrElse {
      toSingleColumnMatrix(c)
    }

  implicit def homoSeqOfHomoSeqsColumnPrepend[
    C, // =:= CT
    A, // =:= CH
    CTag <: SeqTag,
    RTag <: SeqTag,
    RW, // =:= MWH =:= MH
    MW, // =:= MWT =:= M =:= MT
  ](
    implicit
      unconsC: HomoSeqUncons[C, CTag, A],
      consMWH: HomoSeqCons[A, RW, RTag],
      unconsM: HomoSeqUncons[MW, CTag, RW],
      consMW: HomoSeqCons[RW, MW, CTag],
      toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  ): ColumnPrepender[
    C, // C <-
    MW, // M ->
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    recursiveColumnPrepender(
      unconsM = unconsM,
      unconsC = unconsC,
      consMWH = consMWH,
      consMW = consMW,
      toSingleColumnMatrix = toSingleColumnMatrix,
    )

  implicit def homoSeqOfHeteroSeqsColumnPrepender[
    C, // =:= CT
    A, // =:= CH
    M, // =:= MT
    R, // =:= MH
    CTag <: SeqTag,
    RTag <: HeteroSeqTag[_],
    RW, // =:= MWH
    MW, // =:= MWT
  ](
    implicit
      unconsM: HomoSeqUncons[M, CTag, R],
      unconsC: HomoSeqUncons[C, CTag, A],
      consMWH: Cons[A, R, RTag, RW],
      consMW: HomoSeqCons[RW, MW, CTag],
      toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  ): ColumnPrepender[
    C, // C <-
    M, // M <-
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    recursiveColumnPrepender(
      unconsM = unconsM,
      unconsC = unconsC,
      consMWH = consMWH,
      consMW = consMW,
      toSingleColumnMatrix = toSingleColumnMatrix,
    )

  implicit def emptyHeteroSeqColumnPrepender[
    C,
    CTag <: HeteroSeqTag[_],
    RTag <: SeqTag,
    MW, // =:= M
  ](
    implicit
      ev: IsStaticEmpty[C, CTag],
      toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  ): ColumnPrepender[
    C, // C <-
    MW, // M ->
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    columnPrepend(
      unconsM = Uncons.emptySeqUncons[MW, CTag],
      unconsC = Uncons.dummy,
      consMWH = Cons.dummy,
      prependColumnTail = ColumnPrepender.dummy[CTag, RTag],
      consMW = Cons.dummy,
      toSingleColumnMatrix = toSingleColumnMatrix,
    ) _

  implicit def nonEmptyHeteroSeqOfHomoSeqsColumnPrepender[
    C, CH, CT,
    CTag <: HeteroSeqTag[_],
    RTag <: SeqTag,
    MW, // =:= M
    MWH, // =:= MH
    MWT, // =:= MT
  ](
    implicit
      unconsC: Uncons[C, CTag, CH, CT],
      consMWH: HomoSeqCons[CH, MWH, RTag],
      prependColumnTail: ColumnPrepender[CT, MWT, CTag, RTag, MWT],
      unconsM: Uncons[MW, CTag, MWH, MWT],
      consMW: Cons[MWH, MWT, CTag, MW],
  ): ColumnPrepender[
    C, // C <-
    MW, // M ->
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    columnPrepend(
      unconsM = unconsM,
      unconsC = unconsC,
      consMWH = consMWH,
      prependColumnTail = prependColumnTail,
      consMW = consMW,
      toSingleColumnMatrix = ToSingleColumnMatrix.dummy,
    ) _

  implicit def nonEmptyHeteroSeqOfHeteroSeqsColumnPrepender[
    C, CH, CT,
    M, MH, MT,
    CTag <: HeteroSeqTag[_],
    RTag <: HeteroSeqTag[_],
    MW, MWH, MWT,
  ](
    implicit
      unconsM: Uncons[M, CTag, MH, MT],
      unconsC: Uncons[C, CTag, CH, CT],
      consMWH: Cons[CH, MH, RTag, MWH],
      prependColumnTail: ColumnPrepender[CT, MT, CTag, RTag, MWT],
      consMW: Cons[MWH, MWT, CTag, MW],
  ): ColumnPrepender[
    C, // C <-
    M, // M <-
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    columnPrepend(
      unconsM = unconsM,
      unconsC = unconsC,
      consMWH = consMWH,
      prependColumnTail = prependColumnTail,
      consMW = consMW,
      toSingleColumnMatrix = ToSingleColumnMatrix.dummy,
    ) _

  implicit def transposedEmptyHeteroSeqColumnPrepender[
    C,
    CTag <: SeqTag,
    RTag <: HeteroSeqTag[_],
    MW,
  ](
    implicit toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  ): ColumnPrepender[
    C, // C <-
    TransposedEmptyHeteroSeq, // M <-
    CTag, // CTag ->
    RTag, // RTag <-
    MW, // MW ->
  ] =
    columnPrepend(
      unconsM = Uncons.emptySeqUncons[TransposedEmptyHeteroSeq, CTag],
      unconsC = Uncons.dummy,
      consMWH = Cons.dummy,
      prependColumnTail = ColumnPrepender.dummy[CTag, RTag],
      consMW = Cons.dummy,
      toSingleColumnMatrix = toSingleColumnMatrix,
    ) _

  private def recursiveColumnPrepender[
    C, A,
    M, R,
    CTag <: SeqTag,
    RTag <: SeqTag,
    MW, RW,
  ](
    unconsM: HomoSeqUncons[M, CTag, R],
    unconsC: HomoSeqUncons[C, CTag, A],
    consMWH: Cons[A, R, RTag, RW],
    consMW: HomoSeqCons[RW, MW, CTag],
    toSingleColumnMatrix: ToSingleColumnMatrix[C, CTag, RTag, MW],
  ): ColumnPrepender[
    C, // C
    M, // M
    CTag, // CTag
    RTag, // RTag
    MW, // MW
  ] =
    new ColumnPrepender[C, M, CTag, RTag, MW] {
      override def apply(c: C, m: M): MW =
        columnPrepend(
          unconsM = unconsM,
          unconsC = unconsC,
          consMWH = consMWH,
          prependColumnTail = this,
          consMW = consMW,
          toSingleColumnMatrix = toSingleColumnMatrix,
        )(c, m)
    }

  private[transposition] def dummy[
    CTag <: SeqTag,
    RTag <: SeqTag,
  ]: ColumnPrepender[
    Nothing, // C
    Nothing, // M
    CTag, // CTag
    RTag, // RTag
    TransposedEmptyHeteroSeq, // MW
  ] =
    (_: Nothing, _: Nothing) => UnreachableCode_!!!
}
