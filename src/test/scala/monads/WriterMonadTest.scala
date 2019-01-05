package monads

import monads.Monoid._
import monads.WriterMonad._

object WriterMonadTest extends Monad2TestBase[Writer, Vector[String]] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      Writer(Vector("four"), 42),
    )(Writer(Vector("four"), "42"))

  override protected def testPure(): Unit =
    testPure(42, Writer(Vector.empty, 42))

  override protected def testApplicative(): Unit =
    testApplicative(
      Writer(Vector("id"), 7),
      Writer(Vector("name"), "John"),
      Writer(Vector("rating"), 'A'),
    )(Writer(Vector("id", "name", "rating"), User(7, "John", 'A')))

  override protected def testFlatten(): Unit =
    testFlatten(
      Writer(Vector("!"), Writer(Vector("?"), 42)),
      Writer(Vector("!", "?"), 42),
    )

  override protected def testMonad(): Unit =
    testMonad(
      Writer(Vector("id"), 7),
      { case 7 => Writer(Vector("name"), "John"); case _ => Writer(Vector("name-not-found"), "?") },
      { case "John" => Writer(Vector("rating"), 'A'); case _ => Writer(Vector("rating-not-found"), '?') },
    )(Writer(Vector("id", "name", "rating"), User(7, "John", 'A')))

  override protected def testLaws(): Unit = {
    val charToStringWriter = (c: Char) => Writer(Vector("c"), c.toString)
    val stringToIntWriter = (s: String) => Writer(Vector("s"), s.toInt)

    assert(FunctorLaws.checkIdentity(
      Writer(Vector("four"), '4'),
    ))

    assert(FunctorLaws.checkDistributivity(
      Writer(Vector("four"), '4'),
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      Writer(Vector("four"), '4'),
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      Writer(Vector("f"), charToString),
    ))

    assert(ApplicativeLaws.checkComposition(
      Writer(Vector("u"), stringToInt),
      Writer(Vector("v"), charToString),
      Writer(Vector("w"), '4'),
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToStringWriter,
    ))

    assert(MonadLaws.checkRightIdentity(
      Writer(Vector("four"), '4'),
    ))

    assert(MonadLaws.checkAssociativity(
      Writer(Vector("four"), '4'),
      charToStringWriter,
      stringToIntWriter,
    ))
  }
}
