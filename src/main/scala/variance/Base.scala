package variance

import scala.language.higherKinds

class Top
class Middle extends Top
class Bottom extends Middle

trait Base[C[_]] {
  val ofTop: C[Top] = ???
  val ofMiddle: C[Middle] = ???
  val ofBottom: C[Bottom] = ???

  def expectsOfTop(x: C[Top]): Unit = ???
  def expectsOfMiddle(x: C[Middle]): Unit = ???
  def expectsOfBottom(x: C[Bottom]): Unit = ???

  val top = new Top
  val middle = new Middle
  val bottom = new Bottom

  def expectsTop(x: Top): Unit = ???
  def expectsMiddle(x: Middle): Unit = ???
  def expectsBottom(x: Bottom): Unit = ???
}
