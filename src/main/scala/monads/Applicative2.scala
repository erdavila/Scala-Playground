package monads

import monads.HList.Reverse
import scala.language.{higherKinds, implicitConversions}

class Applicative2[F[_, _]] extends Functor2[F] {
  final def pure[X, A](a: A)(implicit impl: Applicative2Impl[F, X]): F[X, A] =
    impl.pure(a)

  final def ap[X, A, B](fa: F[X, A])(f: F[X, A => B])(implicit impl: Applicative2Impl[F, X]): F[X, B] =
    impl.ap(fa)(f)

  implicit class Ops[X, A](fa: F[X, A])(implicit impl: Applicative2Impl[F, X]) extends super.Ops[X, A](fa) {
    def ap[B](f: F[X, A => B]): F[X, B] =
      Applicative2.this.ap(fa)(f)
  }
}

object Applicative2 {
  implicit def apply[F[_, _]]: Applicative2[F] = new Applicative2[F]

  implicit def toOp[F[_, _], X, A](fa: F[X, A]): Op[F, X, HCons[F[X, A], HNil]] =
    new Op[F, X, HCons[F[X, A], HNil]](HCons(fa, HNil))

  class Op[F[_, _], X, FS <: HList](fs: FS) {
    def |@|[B](fb: F[X, B]): Op[F, X, HCons[F[X, B], FS]] =
      new Op(HCons(fb, fs))

    def apply[A, Z, FSR <: HList, R](f: A => Z)(implicit reverse: Reverse[FS, FSR], m: Map[F, X, FSR, A => Z, R]): F[X, R] =
      m(reverse(fs), f)
  }

  sealed trait Map[F[_, _], X, FS, FN, R] {
    def apply(fs: FS, f: FN): F[X, R]
  }

  object Map {
    implicit def m
      [F[_, _]: Applicative2, X, A, FT <: HList, Z, R]
      (implicit tailAp: Ap[F, X, FT, F[X, Z], R], impl: Applicative2Impl[F, X])
    : Map[F, X, HCons[F[X, A], FT], A => Z, R] = new Map[F, X, HCons[F[X, A], FT], A => Z, R] {
      override def apply(fs: HCons[F[X, A], FT], f: A => Z): F[X, R] = {
        val M = Applicative2[F]
        import M.Ops
        val HCons(fa, ft) = fs
        val fz = fa.map(f)
        tailAp(ft, fz)
      }
    }
  }

  sealed trait Ap[F[_, _], X, FS, FF, R] {
    def apply(fs: FS, ff: FF): F[X, R]
  }

  object Ap {
    implicit def singleAp
      [F[_, _]: Applicative2, X, A, R]
      (implicit impl: Applicative2Impl[F, X])
    : Ap[F, X, HCons[F[X, A], HNil], F[X, A => R], R] = new Ap[F, X, HCons[F[X, A], HNil], F[X, A => R], R] {
      override def apply(fs: HCons[F[X, A], HNil], ff: F[X, A => R]): F[X, R] = {
        val M = Applicative2[F]
        import M.Ops
        val HCons(fa, _) = fs
        val fr = fa.ap(ff)
        fr
      }
    }

    implicit def multiAp
      [F[_, _]: Applicative2, X, A, FT <: HList, Z, R]
      (implicit tailAp: Ap[F, X, FT, F[X, Z], R], impl: Applicative2Impl[F, X])
    : Ap[F, X, HCons[F[X, A], FT], F[X, A => Z], R] = new Ap[F, X, HCons[F[X, A], FT], F[X, A => Z], R] {
      override def apply(fs: HCons[F[X, A], FT], ff: F[X, A => Z]): F[X, R] = {
        val M = Applicative2[F]
        import M.Ops
        val HCons(fa, ft) = fs
        val fz = fa.ap(ff)
        val fr = tailAp(ft, fz)
        fr
      }
    }
  }

  class Laws[F[_, _]: Applicative2, X](implicit impl: Applicative2Impl[F, X], run: Run2[F, X]) extends monads.Laws2[F, X] {
    private val M = Applicative2[F]
    import M.Ops

    def checkIdentity[A](fa: F[X, A]): Boolean =
      equivalent(
        fa.ap(M.pure(identity[A](_))),
        fa,
      )

    def checkHomomorphism[A, B](a: A, f: A => B): Boolean =
      equivalent(
        M.pure(a).ap(M.pure(f)),
        M.pure(f(a)),
      )

    def checkInterchange[A, B](a: A, ff: F[X, A => B]): Boolean = {
      val $ = (y: A) => (f: A => B) => f(y)
      equivalent(
        M.pure(a).ap(ff),
        ff.ap(M.pure($(a))),
      )
    }

    def checkComposition[A, B, C](u: F[X, B => C], v: F[X, A => B], w: F[X, A]): Boolean = {
      val compose = (f: B => C) => (g: A => B) => f compose g
      equivalent(
        w.ap(v.ap(u.ap(M.pure(compose)))),
        w.ap(v).ap(u),
      )
    }
  }

  def laws[F[_, _], X](implicit impl: Applicative2Impl[F, X], run: Run2[F, X]): Laws[F, X] = new Laws[F, X]
}
