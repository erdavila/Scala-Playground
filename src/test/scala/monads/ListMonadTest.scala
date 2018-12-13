package monads

import monads.Applicative._
import monads.ListMonad._
import scala.language.higherKinds

object ListMonadTest {

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
    testLift(
      (_: Int).toString,
      List(7, 49),
    )(List("7", "49"))

  private def testLift[F[_]: Functor, A, B](f: A => B, fa: F[A])(expected: F[B]): Unit = {
    val M = Functor[F]
    val g = M.lift(f)
    val result = g(fa)
    assert(result == expected)
  }

  private def testPure(): Unit =
    testPure(42, List(42))

  private def testPure[F[_]: Applicative, A](a: A, expected: F[A]): Unit = {
    val M = Applicative[F]
    val result = M.pure(a)
    assert(result == expected)
  }

  private def testApplicative(): Unit =
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

  private def testApplicative[F[_]: Applicative](
    id: F[Int],
    name: F[String],
    rating: F[Char],
  )(expected: F[User]): Unit = {
    val user = (id |@| name |@| rating)(User.curried)

    assert(user == expected)
  }

  private def testFlatten(): Unit =
    testFlatten(
      List(List("A"), List("B", "C", "D"), List("E", "F")),
      List("A", "B", "C", "D", "E", "F"),
    )

  private def testFlatten[F[_]: Monad, A](fa: F[F[A]], expected: F[A]): Unit = {
    val M = Monad[F]
    import M.Ops
    val result = fa.flatten
    assert(result == expected)
  }

  private def testMonad(): Unit =
    testMonad(
      List(7, 49),
      { case 7 => List("John"); case _ => List() },
      { case "John" => List('A', 'B', 'C'); case _ => List() },
    )(
      List(
        User(7, "John", 'A'), User(7, "John", 'B'), User(7, "John", 'C'),
      )
    )

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
    val chars = List('1', '3')

    val charToString: Char => String = _.toString + "2"
    val stringToInt: String => Int = _.toInt

    val charToStrings = (c: Char) => List(c.toString + "1", c.toString + "2")
    val stringToInts = (s: String) => List(s.toInt)

    val FunctorLaws: Functor.Laws[List] = Functor.laws[List]
    val ApplicativeLaws: Applicative.Laws[List] = Applicative.laws[List]
    val MonadLaws: Monad.Laws[List] = Monad.laws[List]

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
