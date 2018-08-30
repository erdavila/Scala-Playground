package transposition

import scala.language.higherKinds
import shapeless.::
import shapeless.HList
import shapeless.HNil

trait Transposer[M] {
  type Out
  def apply(m: M): Out
}

object Transposer {

  type Aux[M, MX] = Transposer[M] { type Out = MX }

  private def transpose[
    M, MH, MT,
    MTX,
    CTag <: ListTag,
    MX,
  ](
    unconsM: Uncons[M, MH, MT],
    transposeTail: Transposer.Aux[MT, MTX],
    columnPrepend: ColumnPrepender[MH, MTX, CTag, MX],
    emptyMX: Empty[MX],
  )(m: M): MX =
    unconsM(m) map { case (mh, mt) =>
      val mtx = transposeTail(mt)
      columnPrepend(mh, mtx)
    } getOrElse {
      emptyMX()
    }

  implicit def seqTransposer[
    S[X] <: S SeqOf X,
    R,
    MX,
  ](
    implicit
      unconsM: Uncons[S[R], R, S[R]],
      columnPrepend: ColumnPrepender[R, MX, SeqListTag[S], MX],
      emptyMX: Empty[MX],
  ): Transposer.Aux[
    S[R], // M
    MX,   // MX
  ] =
    new Transposer[S[R]] {
      type Out = MX
      def apply(m: S[R]): Out =
        transpose[
          S[R], R, S[R], // M, MH, MT,
          MX,            // MTX,
          SeqListTag[S], // CTag,
          MX,            // MX,
        ](
          unconsM = unconsM,
          transposeTail = this,
          columnPrepend = columnPrepend,
          emptyMX = emptyMX,
        )(m)
    }

  implicit def hnilTransposer(
    implicit
      unconsM: Uncons[HNil, Nothing, Nothing],
      emptyMX: Empty[TransposedHNil],
  ): Transposer.Aux[
    HNil,
    TransposedHNil,
  ] =
    new Transposer[HNil] {
      type Out = TransposedHNil
      def apply(m: HNil): TransposedHNil =
        transpose[
          HNil, Nothing, Nothing, // M, MH, MT,
          Nothing,                // MTX,
          HListListTag,           // CTag,
          TransposedHNil,         // MX,
        ](
          unconsM = unconsM,
          transposeTail = dummy,
          columnPrepend = ColumnPrepender.dummy,
          emptyMX = emptyMX,
        )(m)
    }

  implicit def hconsTransposer[
    MH, MT <: HList,
    MTX,
    MX
  ](
    implicit
      unconsM: Uncons[MH :: MT, MH, MT],
      transposeTail: Transposer.Aux[MT, MTX],
      columnPrepend: ColumnPrepender[MH, MTX, HListListTag, MX],
  ): Transposer.Aux[
    MH :: MT, // M
    MX,       // MX
  ] =
    new Transposer[MH :: MT] {
      type Out = MX
      def apply(m: MH :: MT): Out =
        transpose[
          MH :: MT, MH, MT, // M, MH, MT,
          MTX,              // MTX,
          HListListTag,     // CTag,
          MX,               // MX,
        ](
          unconsM = unconsM,
          transposeTail = transposeTail,
          columnPrepend = columnPrepend,
          emptyMX = Empty.dummy,
        )(m)
    }

  val dummy: Transposer.Aux[
    Nothing, // M
    Nothing, // MX
  ] =
    new Transposer[Nothing] {
      type Out = Nothing
      def apply(m: Nothing): Nothing =
        UnreachableCode_!!!
    }
}
