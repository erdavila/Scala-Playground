package transposition

trait Transposer[M, MX] {
  def apply(m: M): MX
}

object Transposer {

  private def transpose[
    M, MH, MT,
    MTX,
    MTag <: SeqTag, // for M, MT
    MXTag <: SeqTag, // for MX, MH, MTX
    MX,
  ](
    unconsM: Uncons[M, MTag, MH, MT],
    transposeTail: Transposer[MT, MTX],
    columnPrepend: ColumnPrepender[MH, MTX, MXTag, MTag, MX],
    emptyMX: Empty[MXTag, MX],
  )(m: M): MX =
    unconsM(m) map { case (mh, mt) =>
      val mtx = transposeTail(mt)
      columnPrepend(mh, mtx)
    } getOrElse {
      emptyMX()
    }

  implicit def homoSeqTransposer[
    M, // =:= MT
    R, // =:= MH
    MTag <: SeqTag,
    MXTag <: SeqTag,
    MX, // =:= MTX
  ](
    implicit
      unconsM: HomoSeqUncons[M, MTag, R],
      columnPrepend: ColumnPrepender[R, MX, MXTag, MTag, MX],
      emptyMX: Empty[MXTag, MX],
  ): Transposer[
    M, // M <-
    MX, // MX ->
  ] =
    recursiveTransposer(
      unconsM = unconsM,
      columnPrepend = columnPrepend,
      emptyMX = emptyMX,
    )

  implicit def emptyHeteroSeqTransposer[
    M,
    MTag <: HeteroSeqTag[_],
  ](
    implicit ev: IsStaticEmpty[M, MTag],
  ): Transposer[
    M, // M <-
    TransposedEmptyHeteroSeq, // MX ->
  ] =
    transpose(
      unconsM = Uncons.emptySeqUncons[M, MTag],
      transposeTail = Transposer.dummy,
      columnPrepend = ColumnPrepender.dummy[Nothing, MTag],
      emptyMX = () => TransposedEmptyHeteroSeq,
    ) _

  implicit def nonEmptyHeteroSeqTransposer[
    M, MH, MT,
    MTX,
    MTag <: HeteroSeqTag[_],
    MXTag <: SeqTag,
    MX,
  ](
    implicit
      unconsM: Uncons[M, MTag, MH, MT],
      transposeTail: Transposer[MT, MTX],
      columnPrepend: ColumnPrepender[MH, MTX, MXTag, MTag, MX],
  ): Transposer[
    M, // M <-
    MX, // MX ->
  ] =
    transpose(
      unconsM = unconsM,
      transposeTail = transposeTail,
      columnPrepend = columnPrepend,
      emptyMX = Empty.dummy[MXTag, MX],
    ) _

  private def recursiveTransposer[
    M, R,
    MTag <: SeqTag,
    MXTag <: SeqTag,
    MX,
  ](
    unconsM: HomoSeqUncons[M, MTag, R],
    columnPrepend: ColumnPrepender[R, MX, MXTag, MTag, MX],
    emptyMX: Empty[MXTag, MX],
  ): Transposer[
    M, // M
    MX, // MX
  ] =
    new Transposer[M, MX] {
      override def apply(m: M): MX =
        transpose(
          unconsM = unconsM,
          transposeTail = this,
          columnPrepend = columnPrepend,
          emptyMX = emptyMX,
        )(m)
    }

  private val dummy: Transposer[
    Nothing, // M
    Nothing, // MX
  ] =
    (_: Nothing) => UnreachableCode_!!!
}
