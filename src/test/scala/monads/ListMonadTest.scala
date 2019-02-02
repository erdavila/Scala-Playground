package monads

import monads.ListMonad._
import scala.language.higherKinds

object ListMonadTest extends MonadTestBase[List] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      List(7, 49),
    )(List("7", "49"))

  override protected def testPure(): Unit =
    testPure(42, List(42))

  override protected def testApplicative(): Unit =
    testApplicative(
      List(7, 49),
      List("John"),
      List('A', 'B', 'C'),
    )(
      List(
        User(7, "John", 'A'), User(7, "John", 'B'), User(7, "John", 'C'),
        User(49, "John", 'A'), User(49, "John", 'B'), User(49, "John", 'C'),
      )
    )

  override protected def testFlatten(): Unit =
    testFlatten(
      List(List("A"), List("B", "C", "D"), List("E", "F")),
      List("A", "B", "C", "D", "E", "F"),
    )

  override protected def testMonad(): Unit =
    testMonad(
      List(7, 49),
      { case 7 => List("John"); case _ => List() },
      { case "John" => List('A', 'B', 'C'); case _ => List() },
    )(
      List(
        User(7, "John", 'A'), User(7, "John", 'B'), User(7, "John", 'C'),
      )
    )

  override protected def testLaws(): Unit = {
    val chars = List('1', '3')

    val charToStrings = (c: Char) => List(c.toString + "1", c.toString + "2")
    val stringToInts = (s: String) => List(s.toInt)

    assert(FunctorLaws.checkIdentity(
      chars
    ))

    assert(FunctorLaws.checkDistributivity(
      chars,
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      chars
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToStrings,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      List(charToString),
    ))

    assert(ApplicativeLaws.checkComposition(
      List(stringToInt),
      List(charToString),
      List('4', '2'),
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToStrings,
    ))

    assert(MonadLaws.checkRightIdentity(
      chars
    ))

    assert(MonadLaws.checkAssociativity(
      chars,
      charToStrings,
      stringToInts,
    ))
  }
}
