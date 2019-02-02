package mapgeneric

import shapeless._

object MapGeneric {
  def apply[A]: MapGeneric[A] =
    new MapGeneric[A]
}

class MapGeneric[A] {
  def toMap(a: A)(
    implicit toMap: ToMap[A]
  ): Map[String, Any] =
    toMap(a)

  def fromMap[R <: HList](map: Map[String, Any])(
    implicit fromMap: FromMap[A]
  ): Option[A] =
    fromMap(map)
}
