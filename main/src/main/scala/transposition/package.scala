import scala.collection.SeqLike
import scala.language.higherKinds

package object transposition {
  implicit class MatrixOps[M](val m: M) extends AnyVal {
    def transposed[MX](implicit transposer: Transposer.Aux[M, MX]): MX =
      transposer(m)
  }

  type SeqOf[S[_], A] = SeqLike[A, S[A]]

  sealed trait ListTag
  sealed trait SeqListTag[S[X] <: S SeqOf X] extends ListTag
  sealed trait HListListTag extends ListTag

  sealed trait TransposedHNil
  object TransposedHNil extends TransposedHNil

  def UnreachableCode_!!! : Nothing =
    throw new Exception("Supposed unreachable code was reached!")
}
