package monads

import monads.ReaderMonad._

case class E(id: Int, name: String, rating: Char, digit: Char)

object E {
  implicit val instance: E = E(id = 7, name = "John", rating = 'A', digit = '7')
}

object ReaderMonadTest extends Monad2TestBase[Reader, E] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      Reader(_.id),
    )(Reader(_.id.toString))

  override protected def testPure(): Unit =
    testPure(42, Reader(_ => 42))

  override protected def testApplicative(): Unit =
    testApplicative(
      Reader(_.id),
      Reader(_.name),
      Reader(_.rating),
    )(Reader(e => User(e.id, e.name, e.rating)))

  override protected def testFlatten(): Unit =
    testFlatten(
      e1 => e2 => e1.id + e2.id,
      Reader(e => e.id + e.id),
    )

  override protected def testMonad(): Unit =
    testMonad(
      Reader(_.id),
      { case 7 => Reader(_.name); case _ => Reader(_ => "?") },
      { case "John" => Reader(_.rating); case _ => Reader(_ => '?') },
    )(Reader(e => User(e.id, e.name, e.rating)))

  override protected def testLaws(): Unit = {
    val charToStringReader: Char => Reader[E, String] = c => Reader(_.digit + c.toString)
    val stringToIntReader: String => Reader[E, Int] = s => Reader(_.id + s.toInt)

    assert(FunctorLaws.checkIdentity(
      Reader(_.id),
    ))

    assert(FunctorLaws.checkDistributivity(
      Reader(_.digit),
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      Reader(_.id),
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      Reader[E, Char => String](e => c => charToString(c) + e.name),
    ))

    assert(ApplicativeLaws.checkComposition(
      Reader[E, String => Int](e => s => stringToInt(s) + e.id),
      Reader[E, Char => String](e => c => charToString(c) + e.id.toString),
      Reader(_.digit),
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToStringReader,
    ))

    assert(MonadLaws.checkRightIdentity(
      Reader(_.id),
    ))

    assert(MonadLaws.checkAssociativity(
      Reader(_.digit),
      charToStringReader,
      stringToIntReader,
    ))
  }

  override protected def testExtra(): Unit = {
    val doubleDigit: Int => String = for {
      s <- { i: Int => i.toString }
      c <- { i: Int => (i + '0').toChar }
    } yield s + c

    val result = doubleDigit(7)

    assert(result == "77")
  }
}
