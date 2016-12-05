import scala.collection.mutable
import scala.language.implicitConversions
import scala.reflect.ClassTag

object ImplicitChaining extends App {
  implicits
  println()
  explicits

  case class A(x: Int)
  case class B(x: Int, y: Int)
  case class C(x: Int, y: Int, z: Int) {
    def total = x + y + z
  }

  def implicits: Unit = {
    println("With implicits:")

    implicit def intToA(n: Int): A = Trace("intToA", n) {
      A(n)
    }

    implicit def aToB[T : ClassTag](a: T)(implicit f: T => A): B = Trace("aToB", a, f) {
      B(a.x, 2)
    }

    implicit def bToC[T : ClassTag](b: T)(implicit f: T => B): C = Trace("bToC", b, f) {
      C(b.x, b.y, 3)
    }

    // works
    println(1.total)
    println(A(1).total)
    println(B(1, 2).total)
    println(C(1, 2, 3).total)
  }

  def explicits: Unit = {
    println("Without implicits:")

    def intToA(n: Int): A = Trace("intToA", n) {
      A(n)
    }

    def aToB[T : ClassTag](a: T)(f: T => A): B = Trace("aToB", a, f) {
      B(f(a).x, 2)
    }

    def bToC[T : ClassTag](b: T)(f: T => B): C = Trace("bToC", b, f) {
      C(f(b).x, f(b).y, 3)
    }

    println(bToC(1)(i => aToB(i)(intToA)).total)
    println(bToC(A(1))(a => aToB(a)($conforms)).total)
    println(bToC(B(1, 2))($conforms).total)
    println(C(1, 2, 3).total)
  }

  object Trace {
    private val Indentation = "  "
    private var level = 0
    private var lastCalled: Option[String] = None
    private val enabled = mutable.Stack(true)

    private def indentation = Indentation * level

    def apply[T : ClassTag, U, V](name: String, value: T, f: T => U)(func: => V): V = apply(name, value) {
      Trace.identifyType[T]()
      Trace.identifyFunc(f, value)
      func
    }

    def apply[T, R](name: String, value: T)(func: => R): R = {
      println("BEGIN " + name + "(" + value + ": T)")
      val result = indented {
        val result = func
        println("Returning " + result)
        result
      }
      println("END")
      lastCalled = Some(name)
      result
    }

    private def identifyType[T : ClassTag](): Unit =
      println("T is " + implicitly[ClassTag[T]].runtimeClass.getSimpleName())

    private def identifyFunc[T, R](f: T => R, param: T): Unit = {
      lastCalled = None
      enabled.push(false)
      f(param)
      enabled.pop()
      println("f is " + lastCalled.getOrElse("? (probably Predef.$conforms)"))
    }

    private def indented[R](func: => R): R = {
      level += 1
      val result = func
      level -= 1
      result
    }

    private def println(msg: String): Unit = if (enabled.top) Predef.println(indentation + msg)
  }
}
