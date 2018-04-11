package consistenthashing.service

import scala.collection.SortedSet

object CircularSet {

  implicit class Ops[A](sset: SortedSet[A]) {
    def circular: CircularSet[A] =
      new CircularSet(sset)
  }

  def main(args: Array[String]): Unit = {
    testBefore()
    testAfterOrAt()
    println("CircularSet OK")
  }

  private def testBefore(): Unit = {
    {
      val cset = SortedSet('C').circular

      assert(cset.before('B') == 'C')
      assert(cset.before('C') == 'C')
      assert(cset.before('D') == 'C')
    }

    {
      val cset = SortedSet('C', 'E').circular

      assert(cset.before('B') == 'E')
      assert(cset.before('C') == 'E')
      assert(cset.before('D') == 'C')
      assert(cset.before('E') == 'C')
      assert(cset.before('F') == 'E')
    }
  }

  private def testAfterOrAt(): Unit = {
    {
      val sset = SortedSet('C').circular

      assert(sset.afterOrAt('B') == 'C')
      assert(sset.afterOrAt('C') == 'C')
      assert(sset.afterOrAt('D') == 'C')
    }

    {
      val sset = SortedSet('C', 'E').circular

      assert(sset.afterOrAt('B') == 'C')
      assert(sset.afterOrAt('C') == 'C')
      assert(sset.afterOrAt('D') == 'E')
      assert(sset.afterOrAt('E') == 'E')
      assert(sset.afterOrAt('F') == 'C')
    }
  }
}

class CircularSet[A](sset: SortedSet[A]) {

  def before(value: A): A = {
    require(sset.nonEmpty)

    sset.to(value).takeRight(2).toSeq match {
      case Seq(valueBefore, `value`) => valueBefore
      case Seq(_, valueBefore) => valueBefore
      case Seq(`value`) => sset.last
      case Seq(valueBefore) => valueBefore
      case Seq() => sset.last
    }
  }

  def afterOrAt(value: A): A = {
    require(sset.nonEmpty)
    sset.from(value).headOption getOrElse sset.head
  }
}
