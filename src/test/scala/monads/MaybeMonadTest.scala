package monads

import monads.MaybeMonad._
import scala.language.higherKinds

object MaybeMonadTest extends MonadTestBase[Option] {

  override protected def testLift(): Unit =
    for {
      (fa, expected) <- List(
        some(42) -> some("42"),
        none -> none,
      )
    } testLift((_: Int).toString, fa)(expected)

  override protected def testPure(): Unit =
    testPure(42, some(42))

  override protected def testApplicative(): Unit =
    for {
      ((id, name, rating), expected) <- List(
        (some(7), some("John"), some('A')) -> some(User(7, "John", 'A')),
        (some(7), some("John"), none) -> none,
        (some(7), none, some('A')) -> none,
        (some(7), none, none) -> none,
        (none, some("John"), some('A')) -> none,
        (none, some("John"), none) -> none,
        (none, none, some('A')) -> none,
        (none, none, none) -> none,
      )
    } testApplicative(id, name, rating)(expected)

  override protected def testFlatten(): Unit =
    for {
      (fa, expected) <- List(
        some(some(42)) -> some(42),
        some(none) -> none,
        none -> none,
      )
    } testFlatten(fa, expected)

  override protected def testMonad(): Unit =
    testMonad(
      some(7),
      { case 7 => some("John"); case _ => none },
      { case "John" => some('A'); case _ => none },
    )(some(User(7, "John", 'A')))

  override protected def testLaws(): Unit = {
    def maybe[A](a: A) = List(some(a), none)

    val maybeChars = maybe('4')

    val charToMaybeStrings: List[Char => Option[String]] = List(
      c => some(c.toString + "2"),
      _ => none[String],
    )
    val stringToMaybeInts: List[String => Option[Int]] = List(
      s => some(s.toInt),
      _ => none[Int]
    )

    for (fa <- maybeChars)
      assert(FunctorLaws.checkIdentity(fa))

    for (fa <- maybeChars)
      assert(FunctorLaws.checkDistributivity(fa, charToString, stringToInt))

    for (fa <- maybeChars)
      assert(ApplicativeLaws.checkIdentity(fa))

    for (f <- charToMaybeStrings)
      assert(ApplicativeLaws.checkHomomorphism('4', f))

    for (f <- maybe(charToString))
      assert(ApplicativeLaws.checkInterchange('4', f))

    for {
      u <- maybe(stringToInt)
      v <- maybe(charToString)
      w <- maybeChars
    } assert(ApplicativeLaws.checkComposition(u, v, w))

    for (f <- charToMaybeStrings)
      assert(MonadLaws.checkLeftIdentity('4', f))

    for (fa <- maybeChars)
      assert(MonadLaws.checkRightIdentity(fa))

    for {
      fa <- maybeChars
      f <- charToMaybeStrings
      g <- stringToMaybeInts
    } assert(MonadLaws.checkAssociativity(fa, f, g))
  }
}
