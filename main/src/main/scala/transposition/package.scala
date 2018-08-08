import scala.language.higherKinds

package object transposition {

  implicit class MatrixOps[M](val m: M) extends AnyVal {
    def transposed[MX](implicit transposer: Transposer[M, MX]): MX =
      transposer(m)
  }

  sealed trait SeqTag
  sealed trait HomoSeqTag[S[_]] extends SeqTag
  sealed trait HeteroSeqTag[S] extends SeqTag

  final class TransposedEmptyHeteroSeq
  val TransposedEmptyHeteroSeq = new TransposedEmptyHeteroSeq

  type HomoSeqCons[A, S, Tag <: SeqTag] = Cons[A, S, Tag, S]
  type HomoSeqUncons[S, Tag <: SeqTag, A] = Uncons[S, Tag, A, S]

  def UnreachableCode_!!! : Nothing =
    throw new Exception("Supposed unreachable code was reached!")
}
