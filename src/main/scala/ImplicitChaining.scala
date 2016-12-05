import scala.language.implicitConversions
import scala.reflect.ClassTag

object ImplicitChaining extends App {
  case class A(x: Int)
  case class B(x: Int, y: Int)
  case class C(x: Int, y: Int, z: Int) {
    def total = x + y + z
  }

  implicit def toA(n: Int) = A(n)
  implicit def aToB[A1 : ClassTag](a: A1)(implicit f: A1 => A) = B(a.x, 2)
  implicit def bToC[B1 : ClassTag](b: B1)(implicit f: B1 => B) = C(b.x, b.y, 3)

  // works
  println(1.total)
  println(A(1).total)
  println(B(1, 2).total)
  println(C(1, 2, 3).total)
}
