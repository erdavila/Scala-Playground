package monads

import monads.Applicative._
import monads.MaybeMonad._
import scala.language.higherKinds

object MaybeMonadTest {

  def main(args: Array[String]): Unit = {
    testLift()
    testPure()
    testApplicative()
    testFlatten()
    testMonad()
    testLaws()
    println("OK " + getClass.getName)
  }

  private def testLift(): Unit =
    for {
      (fa, expected) <- List(
        some(42) -> some("42"),
        none -> none,
      )
    } testLift((_: Int).toString, fa)(expected)

  private def testLift[F[_]: Functor, A, B](f: A => B, fa: F[A])(expected: F[B]): Unit = {
    val M = Functor[F]
    val g = M.lift(f)
    val result = g(fa)
    assert(result == expected)
  }

  private def testPure(): Unit =
    testPure(42, some(42))

  private def testPure[F[_]: Applicative, A](a: A, expected: F[A]): Unit = {
    val M = Applicative[F]
    val result = M.pure(a)
    assert(result == expected)
  }

  private def testApplicative(): Unit =
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

  private def testApplicative[F[_]: Applicative](
    id: F[Int],
    name: F[String],
    rating: F[Char],
  )(expected: F[User]): Unit = {
    val user = (id |@| name |@| rating)(User.curried)

    assert(user == expected)
  }

  private def testFlatten(): Unit =
    for {
      (fa, expected) <- List(
        some(some(42)) -> some(42),
        some(none) -> none,
        none -> none,
      )
    } testFlatten(fa, expected)

  private def testFlatten[F[_]: Monad, A](fa: F[F[A]], expected: F[A]): Unit = {
    val M = Monad[F]
    import M.Ops
    val result = fa.flatten
    assert(result == expected)
  }

  private def testMonad(): Unit =
    testMonad(
      some(7),
      { case 7 => some("John"); case _ => none },
      { case "John" => some('A'); case _ => none },
    )(some(User(7, "John", 'A')))

  private def testMonad[F[_]: Monad](
    id: F[Int],
    getNameById: Int => F[String],
    getRatingByName: String => F[Char],
  )(expected: F[User]): Unit = {
    val M = Monad[F]
    import M.Ops

    val user = for {
      id <- id
      name <- getNameById(id)
      rating <- getRatingByName(name)
    } yield User(id, name, rating)

    assert(user == expected)
  }

  private def testLaws(): Unit = {
    def maybe[A](a: A) = List(some(a), none)

    val maybeChars = maybe('4')

    val charToString: Char => String = _.toString + "2"
    val stringToInt: String => Int = _.toInt

    val charToMaybeStrings: List[Char => Option[String]] = List(
      c => some(c.toString + "2"),
      _ => none[String],
    )

    val stringToMaybeInts: List[String => Option[Int]] = List(
      s => some(s.toInt),
      _ => none[Int]
    )

    val FunctorLaws: Functor.Laws[Option] = Functor.laws[Option]
    val ApplicativeLaws: Applicative.Laws[Option] = Applicative.laws[Option]
    val MonadLaws: Monad.Laws[Option] = Monad.laws[Option]

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
