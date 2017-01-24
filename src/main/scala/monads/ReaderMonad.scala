package monads

object ReaderMonad extends Monad2Companion[ReaderMonad] {
  def `return`[E, A](a: A): M[E, A] = { _ => a }

  object implicits {
    implicit class ReaderMonadWrapper[E, A](ma: E => A) extends ScalaMonad2[M, E, A] {
      def >>=[B](f: A => M[E, B]): M[E, B] =
        { e => f(ma(e))(e) }

      def local[EE](extract: EE => E): EE => A = { extract andThen ma }

      def _companion = ReaderMonad
    }
  }
}

object ReaderMonadTest extends App {
  import ReaderMonad.implicits._
  import Monad.implicits._

  assert {
    val m: Char => Int = ReaderMonad.`return`(7)
    m('@') == 7
  }

  assert {
    val m = { (x: Int) => x / 2.0 } >>= { n => (x: Int) => (n + x).toString() }
    m(7) == "10.5"
  }

  assert {
    val m = { (x: Int) => x / 2.0 } fmap { n => n.toString() }
    m(7) == "3.5"
  }

  assert {
    val mm: Int => Int => String = { x => y => (x + y).toString() }
    val m: Int => String = mm.join()
    m(7) == "14"
  }

  assert {
    val power = { n: Int => x: Int => 1 to n map { _ => x } reduce { _ * _ } }
    val product = { n: Int => x: Int => n * x }
    val m = power >=> product
    val X = 3
    val Y = 4
    m(Y)(X) == product(X)(power(Y)(X))
  }

  assert {
    val s = (_: Int) + (_: Double)
    val lift = ReaderMonad.liftM2(s)
    val m1 = { x: Int => x * x }
    val m2 = { x: Int => 1.5 * x }
    val m = lift[Int](m1, m2)
    val X = 4
    m(X) == s(m1(X), m2(X))
  }

  trait DataStore { def runQuery(query: String): List[String] }
  trait EmailServer { def sendEmail(to: String, content: String): Unit }
  object FindUsers {
    def inactive: DataStore => () => List[String] =
      dataStore => () => dataStore.runQuery("select inactive")
  }
  object UserReminder {
    def emailInactive(inactive: () => List[String]): EmailServer => () => Unit =
      emailServer => () => inactive().foreach(emailServer.sendEmail(_, "We miss you"))
  }
  object CustomerRelations {
    def retainUsers(emailInactive: () => Unit): () => Unit =
      () => {
        println("emailing inactive users")
        emailInactive()
      }
  }

  case class Config(dataStore: DataStore, emailServer: EmailServer)
  val config = Config(
    new DataStore { def runQuery(query: String) = List("john.doe@example.com") },
    new EmailServer { def sendEmail(to: String, content: String) = println(s"sending [$content] to $to") }
  )

  {
    val reader = for {
      getAddresses <- FindUsers.inactive.local[Config](_.dataStore)
      emailInactive <- UserReminder.emailInactive(getAddresses).local[Config](_.emailServer)
    } yield CustomerRelations.retainUsers(emailInactive)
    reader(config)()
  }

  {
    val reader = FindUsers.inactive.local[Config](_.dataStore) >>= { getAddresses =>
      UserReminder.emailInactive(getAddresses).local[Config](_.emailServer) >>= { emailInactive =>
        ReaderMonad.`return`(CustomerRelations.retainUsers(emailInactive))
      }
    }
    reader(config)()
  }

  println("OK")
}
