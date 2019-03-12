package mapfoldleft

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object MapPlusVar {
  implicit class Ops[CC[X] <: TraversableLike[X, CC[X]], A](val as: CC[A]) extends AnyVal {
    def mapFoldLeft[S, B, That](z: S)(f: (S, A) => (B, S))(implicit bf: CanBuildFrom[CC[A], B, That]): (S, That) = {
      var s = z

      val bs = as.map { a =>
        val (b, ss) = f(s, a)
        s = ss
        b
      }

      (s, bs)
    }
  }
}
