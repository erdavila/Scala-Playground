package booleanimplicits

final case class Xor[L, R](value: Either[L, R])

object Xor {
  implicit def xor[L, R](
    implicit or:
      (L And Not[R])
      Or
      (Not[L] And R)
  ): Xor[L, R] = {
    val l = or.left.map(lAndNotR => Left(lAndNotR.left))
    val r = or.right.map(notLAndR => Right(notLAndR.right))
    val Seq(value) = l ++ r
    Xor[L, R](value)
  }
}
