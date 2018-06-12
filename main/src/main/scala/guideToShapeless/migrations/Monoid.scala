package guideToShapeless.migrations

trait Monoid[A] {
  def empty: A
  def combine(x: A, y: A): A
}

object Monoid {
  def createMonoid[A](zero: A)(add: (A, A) => A): Monoid[A] =
    new Monoid[A] {
      def empty = zero
      def combine(x: A, y: A): A = add(x, y)
    }

  implicit val intMonoid: Monoid[Int] =
    createMonoid(0) { _ + _ }
}
