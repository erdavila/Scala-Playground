package booleanimplicits

final case class And[L, R](left: L, right: R)

object And {
  implicit def both[L, R](implicit l: L, r: R): And[L, R] =
    And[L, R](l, r)
}
