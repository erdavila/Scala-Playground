package mapfoldleft

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

object FoldLeft {
  implicit class Ops[CC[X] <: TraversableLike[X, CC[X]], A](val as: CC[A]) extends AnyVal {
    def mapFoldLeft[S, B, That](z: S)(f: (S, A) => (B, S))(implicit bf: CanBuildFrom[CC[A], B, That]): (S, That) = {
      val builder = bf(as)

      val (finalState, `builder`) = as.foldLeft((z, builder)) { case ((s, bldr), a) =>
        val (b, ss) = f(s, a)
        bldr += b
        (ss, bldr)
      }

      val bs = builder.result()
      (finalState, bs)
    }
  }
}
