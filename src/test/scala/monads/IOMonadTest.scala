package monads

import monads.IOMonad._
import monads.Run.run
import scala.collection.mutable.Buffer

object IOMonadTest extends MonadTestBase[IO] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      io(42),
    )(io("42"))

  override protected def testPure(): Unit =
    testPure(42, io(42))

  override protected def testApplicative(): Unit =
    testApplicative(
      io(7),
      io("John"),
      io('A'),
    )(io(User(7, "John", 'A')))

  override protected def testFlatten(): Unit =
    testFlatten(io(io(42)), io(42))

  override protected def testMonad(): Unit =
    testMonad(
      io(7),
      { case 7 => io("John"); case _ => io("?") },
      { case "John" => io('A'); case _ => io('?') },
    )(io(User(7, "John", 'A')))

  override protected def testExtra(): Unit =
    testLaziness()

  private def testLaziness(): Unit = {
    val log = Buffer.empty[String]
    def getId = io {
      log += "got id"
      7
    }
    def getNameById(id: Int) = io {
      log += "got name"
      "John"
    }

    import M.Ops

    val name = for {
      id <- getId
      name <- getNameById(id)
    } yield name

    assert(log.isEmpty)
    val result = run(name)
    assert(log == Seq("got id", "got name"))
    assert(result == "John")
  }

  override protected def testLaws(): Unit = {
    val charToStringIO = (c: Char) => io { c.toString }

    val stringToIntIO = (s: String) => io { s.toInt }

    assert(FunctorLaws.checkIdentity(
      io(42)
    ))

    assert(FunctorLaws.checkDistributivity(
      io('4'),
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      io('4')
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      io(charToString),
    ))

    assert(ApplicativeLaws.checkComposition(
      io(stringToInt),
      io(charToString),
      io('4'),
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToStringIO,
    ))

    assert(MonadLaws.checkRightIdentity(
      io('4'),
    ))

    assert(MonadLaws.checkAssociativity(
      io('4'),
      charToStringIO,
      stringToIntIO,
    ))
  }
}
