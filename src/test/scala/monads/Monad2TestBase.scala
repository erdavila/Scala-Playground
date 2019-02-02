package monads

import monads.Applicative2.toOp
import scala.language.higherKinds

abstract class Monad2TestBase[F[_, _]: Monad2, X](implicit impl: Monad2Impl[F, X], run: Run2[F, X]) {

  protected val M = Monad2[F]
  import M.Ops

  final def main(args: Array[String]): Unit = {
    testLift()
    testPure()
    testApplicative()
    testFlatten()
    testMonad()
    testLaws()
    testExtra()
    println("OK " + getClass.getName)
  }

  protected def testLift(): Unit

  protected def testLift[A, B](f: A => B, fa: F[X, A])(expected: F[X, B]): Unit = {
    val g = M.lift[X, A, B](f)
    val result = g(fa)
    assert(run(result) == run(expected))
  }

  protected def testPure(): Unit

  protected def testPure[A](a: A, expected: F[X, A]): Unit = {
    val result = M.pure[X, A](a)
    assert(run(result) == run(expected))
  }

  protected def testApplicative(): Unit

  protected def testApplicative(
    id: F[X, Int],
    name: F[X, String],
    rating: F[X, Char],
  )(expected: F[X, User]): Unit = {
    val user = (id |@| name |@| rating)(User.curried)

    assert(run(user) == run(expected))
  }

  protected def testFlatten(): Unit

  protected def testFlatten[A](fa: F[X, F[X, A]], expected: F[X, A]): Unit = {
    val result = fa.flatten
    assert(run(result) == run(expected))
  }

  protected def testMonad(): Unit

  protected def testMonad(
    id: F[X, Int],
    getNameById: Int => F[X, String],
    getRatingByName: String => F[X, Char],
  )(expected: F[X, User]): Unit = {
    val user = for {
      id <- id
      name <- getNameById(id)
      rating <- getRatingByName(name)
    } yield User(id, name, rating)

    assert(run(user) == run(expected))
  }

  protected def testExtra(): Unit = {}

  protected val charToString: Char => String = _.toString + "2"
  protected val stringToInt: String => Int = _.toInt

  protected val FunctorLaws: Functor2.Laws[F, X] = Functor2.laws[F, X]
  protected val ApplicativeLaws: Applicative2.Laws[F, X] = Applicative2.laws[F, X]
  protected val MonadLaws: Monad2.Laws[F, X] = Monad2.laws[F, X]

  protected def testLaws(): Unit
}
