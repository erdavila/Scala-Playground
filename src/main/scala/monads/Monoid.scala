package monads

trait Monoid[A] {
  def zero: A
  def append(x: A, y: A): A

  implicit class Ops(x: A) {
    def |+|(y: A): A = append(x, y)
  }
}

object Monoid {
  implicit def vectorMonoid[A]: Monoid[Vector[A]] = new Monoid[Vector[A]] {
    override def zero: Vector[A] =
      Vector.empty

    override def append(x: Vector[A], y: Vector[A]): Vector[A] =
      x ++ y
  }
}
