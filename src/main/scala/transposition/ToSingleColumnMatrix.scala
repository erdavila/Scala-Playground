package transposition

trait ToSingleColumnMatrix[S, STag <: SeqTag, RTag <: SeqTag, M] {
  def apply(s: S): M
}

object ToSingleColumnMatrix {

  private def toSingleColumnMatrix[
    S, SH, ST,
    STag <: SeqTag, // for S, ST, M, MT
    RTag <: SeqTag, // for MH
    M, MH, MT,
  ](
    unconsS: Uncons[S, STag, SH, ST],
    singleMH: Single[SH, RTag, MH],
    tailToSingleColumnMatrix: ToSingleColumnMatrix[ST, STag, RTag, MT],
    consM: Cons[MH, MT, STag, M],
    emptyM: Empty[STag, M],
  )(s: S): M =
    unconsS(s) map { case (sh, st) =>
      val mh = singleMH(sh)
      val mt = tailToSingleColumnMatrix(st)
      consM(mh, mt)
    } getOrElse {
      emptyM()
    }

  implicit def homoSeqToSingleColumnMatrix[
    S, // =:= ST
    A, // =:= SH
    STag <: SeqTag,
    RTag <: SeqTag,
    R, // =:= MH
    M, // =:= MT
  ](
    implicit
      unconsS: HomoSeqUncons[S, STag, A],
      singleMH: Single[A, RTag, R],
      consM: HomoSeqCons[R, M, STag],
      emptyM: Empty[STag, M],
  ): ToSingleColumnMatrix[
    S, // S <-
    STag, // STag <-
    RTag, // RTag <-
    M, // M <-
  ] =
    recursiveToSingleColumnMatrix(
      unconsS = unconsS,
      singleMH = singleMH,
      consM = consM,
      emptyM = emptyM,
    )

  implicit def emptyHomoSeqOfMultiColumnHeteroSeqToSingleColumnMatrix[
    S, // =:= ST
    A, // =:= SH
    STag <: SeqTag,
    RTag <: HeteroSeqTag[_],
    R, // =:= MH
    RT,
    M,
  ](
    implicit
      unconsS: HomoSeqUncons[S, STag, A],
      consM: HomoSeqCons[R, M, STag],
      unconsR: Uncons[R, RTag, A, RT],
      unconsRT: Uncons[RT, RTag, _, _],
      emptyM: Empty[STag, M],
  ): ToSingleColumnMatrix[
    S, // S <-
    STag, // STag <-
    RTag, // RTag <-
    M, // M <-
  ] =
    toSingleColumnMatrix(
      unconsS = unconsS,
      singleMH = Single.dummy[A],
      tailToSingleColumnMatrix = ToSingleColumnMatrix.dummy,
      consM = Cons.dummy,
      emptyM = emptyM,
    ) _

  implicit def emptyHeteroSeqToSingleColumnMatrix[
    S,
    STag <: HeteroSeqTag[_],
    RTag <: SeqTag,
    M,
  ](
    implicit
      ev: IsStaticEmpty[S, STag],
      emptyM: Empty[STag, M],
  ): ToSingleColumnMatrix[
    S, // S <-
    STag, // STag <-
    RTag, // RTag <-
    M, // M <-
  ] =
    toSingleColumnMatrix(
      unconsS = Uncons.emptySeqUncons[S, STag],
      singleMH = Single.dummy,
      tailToSingleColumnMatrix = ToSingleColumnMatrix.dummy,
      consM = Cons.dummy,
      emptyM = emptyM,
    ) _

  implicit def nonEmptyHeteroSeqToSingleColumnMatrix[
    S, SH, ST,
    STag <: HeteroSeqTag[_],
    RTag <: SeqTag,
    M, MH, MT,
  ](
    implicit
      unconsS: Uncons[S, STag, SH, ST],
      singleMH: Single[SH, RTag, MH],
      tailToSingleColumnMatrix: ToSingleColumnMatrix[ST, STag, RTag, MT],
      consM: Cons[MH, MT, STag, M],
  ): ToSingleColumnMatrix[
    S, // S <-
    STag, // STag ->
    RTag, // RTag <-
    M, // M ->
  ] =
    toSingleColumnMatrix(
      unconsS = unconsS,
      singleMH = singleMH,
      tailToSingleColumnMatrix = tailToSingleColumnMatrix,
      consM = consM,
      emptyM = Empty.dummy,
    ) _

  private def recursiveToSingleColumnMatrix[
    S, A,
    STag <: SeqTag,
    RTag <: SeqTag,
    M, R,
  ](
    unconsS: HomoSeqUncons[S, STag, A],
    singleMH: Single[A, RTag, R],
    consM: HomoSeqCons[R, M, STag],
    emptyM: Empty[STag, M],
  ): ToSingleColumnMatrix[
    S, // S
    STag, // STag
    RTag, // MTag
    M, // M
  ] =
    new ToSingleColumnMatrix[S, STag, RTag, M] {
      override def apply(s: S): M =
        toSingleColumnMatrix(
          unconsS = unconsS,
          singleMH = singleMH,
          tailToSingleColumnMatrix = this,
          consM = consM,
          emptyM = emptyM,
        )(s)
    }

  private[transposition] def dummy[
    S,
    STag <: SeqTag,
    RTag <: SeqTag,
    M,
  ]: ToSingleColumnMatrix[
    S, // S
    STag, // STag
    RTag, // RTag
    M, // M
  ] =
    (_: S) => UnreachableCode_!!!
}
