package monads

import monads.StateMonad._

case class S(id: Int, name: String, rating: Char, log: Vector[String]) {
  def withLog(l: String): S = copy(log = log :+ l)
}

object S {
  implicit val instance: S = S(id = 7, name = "John", rating = 'A', log = Vector("."))
}

object StateMonadTest extends Monad2TestBase[State, S] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      State(s => (s.id, s.withLog("got id")))
    )(State(s => (s.id.toString, s.withLog("got id"))))

  override protected def testPure(): Unit =
    testPure(42, State(s => (42, s)))

  override protected def testApplicative(): Unit =
    testApplicative(
      State { s => (s.id, s.withLog("got id")) },
      State { s => (s.name, s.withLog("got name")) },
      State { s => (s.rating, s.withLog("got rating")) },
    )(State { s =>
      val user = User(s.id, s.name, s.rating)
      val newS = s.copy(log = s.log ++ Vector("got id", "got name", "got rating"))
      (user, newS)
    })

  override protected def testFlatten(): Unit =
    testFlatten(
      State { s1 =>
        val st = State { s2: S =>
          val id = s1.id + s2.id
          val newS = s1.copy(log = s1.log ++ Vector(",") ++ s2.log)
          (id, newS)
        }
        (st, s1.withLog("flattened"))
      },
      State { s =>
        val id = s.id + s.id
        val newS = s.copy(log = s.log ++ Vector(",") ++ s.log :+ "flattened")
        (id, newS)
      },
    )

  override protected def testMonad(): Unit =
    testMonad(
      State { s => (s.id, s.withLog("got id")) },
      id => State { s => (s.name, s.withLog("got name by id " + id)) },
      name => State { s => (s.rating, s.withLog("got rating by name " + name)) },
    )(State { s =>
      val user = User(s.id, s.name, s.rating)
      val newS = s.copy(log = s.log ++ Vector("got id", "got name by id " + s.id, "got rating by name " + s.name))
      (user, newS)
    })

  override protected def testLaws(): Unit = {
    val charToStringState: Char => State[S, String] = (c: Char) => State(s => (c.toString, s.withLog("charToString")))
    val stringToIntState: String => State[S, Int] = (str: String) => State(s => (str.toInt, s.withLog("stringToInt")))

    assert(FunctorLaws.checkIdentity(
      State(s => (s.id, s.withLog("got id"))),
    ))

    assert(FunctorLaws.checkDistributivity(
      State(s => ('7', s.withLog("char '7'"))),
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      State(s => (s.id, s.withLog("got id"))),
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '7',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '7',
      State { s =>
        (charToString, s.withLog("charToString"))
      }
    ))

    assert(ApplicativeLaws.checkComposition(
      State(s => (stringToInt, s.withLog("stringToInt"))),
      State(s => (charToString, s.withLog("charToString"))),
      State(s => ('7', s.withLog("char '7'"))),
    ))

    assert(MonadLaws.checkLeftIdentity(
      '7',
      charToStringState,
    ))

    assert(MonadLaws.checkRightIdentity(
      State(s => (s.id, s.withLog("got id"))),
    ))

    assert(MonadLaws.checkAssociativity(
      State(s => ('7', s.withLog("char '7'"))),
      charToStringState,
      stringToIntState,
    ))
  }

  override protected def testExtra(): Unit = {
    import M.Ops

    def push[A](top: A): State[List[A], Unit] = s => ((), top :: s)
    def pop[A]: State[List[A], A] = s => (s.head, s.tail)

    def swapTops[A]: State[List[A], Unit] = for {
      x <- pop[A]
      y <- pop[A]
      _ <- push(x)
      _ <- push(y)
    } yield ()

    def reverseAndPush[A](value: A): State[List[A], Unit] = for {
      _ <- modify[List[A]](_.reverse)
      s <- get[List[A]]
      _ <- put(value :: s)
    } yield ()

    val mutate = for {
      _ <- swapTops[Int]
      _ <- reverseAndPush(0)
    } yield ()

    val ((), result) = mutate(List(1, 2, 3, 4, 5))

    assert(result == List(0, 5, 4, 3, 1, 2))
  }
}
