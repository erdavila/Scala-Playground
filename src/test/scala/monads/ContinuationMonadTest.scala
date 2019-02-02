package monads

import monads.ContinuationMonad._

object ContinuationMonadTest extends MonadTestBase[Cont] {

  override protected def testLift(): Unit =
    testLift(
      (_: Int).toString,
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(42) },
    )(new Cont[String] { override def apply[R](c: Continuation[String, R]): R = c("42") })

  override protected def testPure(): Unit =
    testPure(
      42,
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(42) }
    )

  override protected def testApplicative(): Unit =
    testApplicative(
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(7) },
      new Cont[String] { override def apply[R](c: Continuation[String, R]): R = c("John") },
      new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('A') },
    )(new Cont[User] { override def apply[R](c: Continuation[User, R]): R = c(User(7, "John", 'A')) })

  override protected def testFlatten(): Unit =
    testFlatten(
      new Cont[Cont[Int]] {
        override def apply[R](c: Continuation[Cont[Int], R]): R = {
          val cont = new Cont[Int] { override def apply[RR](cc: Continuation[Int, RR]): RR = cc(42): RR }
          c(cont): R
        }
      },
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(42) },
    )

  override protected def testMonad(): Unit =
    testMonad(
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(7) },
      {
        case 7 => new Cont[String] { override def apply[R](c: Continuation[String, R]): R = c("John") }
        case _ => new Cont[String] { override def apply[R](c: Continuation[String, R]): R = c("?") }
      },
      {
        case "John" => new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('A') }
        case _      => new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('?') }
      },
    )(new Cont[User] { override def apply[R](c: Continuation[User, R]): R = c(User(7, "John", 'A')) })

  override protected def testLaws(): Unit = {
    val charToStringContinuation =
      (ch: Char) =>
        new Cont[String] { override def apply[R](c: Continuation[String, R]): R = c(ch.toString) }

    val stringToIntContinuation =
      (s: String) =>
        new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(s.toInt) }

    assert(FunctorLaws.checkIdentity(
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(42) }
    ))

    assert(FunctorLaws.checkDistributivity(
      new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('4') },
      charToString,
      stringToInt,
    ))

    assert(ApplicativeLaws.checkIdentity(
      new Cont[Int] { override def apply[R](c: Continuation[Int, R]): R = c(42) }
    ))

    assert(ApplicativeLaws.checkHomomorphism(
      '4',
      charToString,
    ))

    assert(ApplicativeLaws.checkInterchange(
      '4',
      new Cont[Char => String] { override def apply[R](c: Continuation[Char => String, R]): R = c(charToString) }
    ))

    assert(ApplicativeLaws.checkComposition(
      new Cont[String => Int] { override def apply[R](c: Continuation[String => Int, R]): R = c(stringToInt) },
      new Cont[Char => String] { override def apply[R](c: Continuation[Char => String, R]): R = c(charToString) },
      new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('4') },
    ))

    assert(MonadLaws.checkLeftIdentity(
      '4',
      charToStringContinuation,
    ))

    assert(MonadLaws.checkRightIdentity(
      new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('4') },
    ))

    assert(MonadLaws.checkAssociativity(
      new Cont[Char] { override def apply[R](c: Continuation[Char, R]): R = c('4') },
      charToStringContinuation,
      stringToIntContinuation,
    ))
  }

  // From https://en.wikipedia.org/w/index.php?title=Continuation-passing_style&oldid=877453434#CPS_in_Haskell
  override protected def testExtra(): Unit = {
    def pow2M(x: Double): Cont[Double] =
      M.unit(math.pow(x, 2.0))

    def addCps[R](x: Double, y: Double)(c: Continuation[Double, R]): R =
      c(x + y)

    def sqrtCps[R](x: Double)(c: Continuation[Double, R]): R =
      c(math.sqrt(x))

    def vectorLength(x: Double, y: Double): Cont[Double] =
      callCC[Double, Double] { exitF =>
        import M.Ops
        for {
          _ <- if (x < 0 || y < 0) exitF(0.0) else M.unit(())
          x2 <- pow2M(x)
          y2 <- pow2M(y)
          xny <- new Cont[Double] { override def apply[R](c: Continuation[Double, R]): R = addCps(x2, y2)(c) }
          r <-   new Cont[Double] { override def apply[R](c: Continuation[Double, R]): R = sqrtCps(xny)(c) }
        } yield r
      }

    assert(vectorLength( 3.0,  4.0)(identity) == 5.0)
    assert(vectorLength(-3.0,  4.0)(identity) == 0.0)
    assert(vectorLength( 3.0, -4.0)(identity) == 0.0)
  }
}
