package monads

import monads.Applicative.toOp
import monads.Run.run
import scala.language.higherKinds

abstract class MonadTestBase[F[_]: Monad: Run] {

  protected val M: Monad[F] = Monad[F]
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

  protected final def testLift[A, B](f: A => B, fa: F[A])(expected: F[B]): Unit = {
    val g = M.lift(f)
    val result = g(fa)
    assert(run(result) == run(expected))
  }

  protected def testPure(): Unit

  protected final def testPure[A](a: A, expected: F[A]): Unit = {
    val result = M.pure(a)
    assert(run(result) == run(expected))
  }

  protected def testApplicative(): Unit

  protected final def testApplicative(
    id: F[Int],
    name: F[String],
    rating: F[Char],
  )(expected: F[User]): Unit = {
    val user = (id |@| name |@| rating)(User.curried)

    assert(run(user) == run(expected))
  }

  protected def testFlatten(): Unit

  protected final def testFlatten[A](fa: F[F[A]], expected: F[A]): Unit = {
    val result = fa.flatten
    assert(run(result) == run(expected))
  }

  protected def testMonad(): Unit

  protected final def testMonad(
    id: F[Int],
    getNameById: Int => F[String],
    getRatingByName: String => F[Char],
  )(expected: F[User]): Unit = {
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

  protected val FunctorLaws: Functor.Laws[F] = Functor.laws[F]
  protected val ApplicativeLaws: Applicative.Laws[F] = Applicative.laws[F]
  protected val MonadLaws: Monad.Laws[F] = Monad.laws[F]

  protected def testLaws(): Unit
}
