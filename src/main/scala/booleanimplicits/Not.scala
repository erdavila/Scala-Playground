package booleanimplicits

trait Not[+V]

object Not extends Not[Nothing] {

  implicit def not[V](implicit x: AvoidAmbiguityErrorMessage[V]): Not[V] = Not

  trait AvoidAmbiguityErrorMessage[+V]
  private object AvoidAmbiguityErrorMessage extends AvoidAmbiguityErrorMessage[Nothing]
  implicit def always[V]: AvoidAmbiguityErrorMessage[V] = AvoidAmbiguityErrorMessage
  implicit def whenImplicitIsSatisfied[V](implicit ev: V): AvoidAmbiguityErrorMessage[V] = AvoidAmbiguityErrorMessage
}
