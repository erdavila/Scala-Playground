package booleanimplicits

final case class Or[L, R](left: Option[L], right: Option[R])

trait LowPriorityOr {
  implicit def leftOnly[L, R](implicit l: L): Or[L, R] =
    Or(Some(l), None)

  implicit def rightOnly[L, R](implicit r: R): Or[L, R] =
    Or(None, Some(r))
}

object Or extends LowPriorityOr {
  implicit def both[L, R](implicit l: L, r: R): Or[L, R] =
    Or(Some(l), Some(r))
}
