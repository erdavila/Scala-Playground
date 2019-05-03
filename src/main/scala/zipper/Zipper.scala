package zipper

object Zipper {
  def apply[A](focus: A): Option[Zipper[A]] =
    Some(Zipper(focus, path = Nil))
}

case class Zipper[A](focus: A, path: List[A => A]) {
  def goDown(newFocus: A, restore: A => A): Zipper[A] =
    Zipper(newFocus, restore :: path)

  def updateTo(newFocus: A): Zipper[A] =
    Zipper(newFocus, path)

  def goUp: Option[Zipper[A]] =
    path match {
      case Nil => None
      case restore :: newTail => Some(Zipper(restore(focus), newTail))
    }

  def goTop: Zipper[A] =
    goUp.fold(this)(_.goTop)
}
