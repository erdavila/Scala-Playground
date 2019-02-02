package monads

import monads.IdentityMonad._

object IdentityMonadTest extends MonadTestBase[Identity] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      42,
    )("42")

  override protected def testPure(): Unit =
    testPure(42, 42)

  override protected def testApplicative(): Unit =
    testApplicative(
      7,
      "John",
      'A',
    )(User(7, "John", 'A'))

  override protected def testFlatten(): Unit =
    testFlatten(42, 42)

  override protected def testMonad(): Unit =
    testMonad(
      7,
      { case 7 => "John"; case _ => "?" },
      { case "John" => 'A'; case _ => '?' },
    )(User(7, "John", 'A'))

  override protected def testLaws(): Unit = {
    assert(FunctorLaws.checkIdentity(
      42
    ))

    assert(FunctorLaws.checkDistributivity(
      '4',
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      '4'
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkComposition(
      stringToInt,
      charToString,
      '4',
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToString,
    ))

    assert(MonadLaws.checkRightIdentity(
      '4',
    ))

    assert(MonadLaws.checkAssociativity(
      '4',
      charToString,
      stringToInt,
    ))
  }
}
