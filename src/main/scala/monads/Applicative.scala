package monads

import monads.HList.Reverse
import scala.language.{higherKinds, implicitConversions}

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(ff: F[A => B]): F[B]

  implicit class Ops[A](fa: F[A]) extends super.Ops[A](fa) {
    def ap[B](f: F[A => B]): F[B] =
      Applicative.this.ap(fa)(f)
  }
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  implicit def toOp[F[_], A](fa: F[A]): Op[F, HCons[F[A], HNil]] =
    new Op[F, HCons[F[A], HNil]](HCons(fa, HNil))

  class Op[F[_], FS <: HList](fs: FS) {
    def |@|[B](fb: F[B]): Op[F, HCons[F[B], FS]] =
      new Op(HCons(fb, fs))

    def apply[A, Z, FSR <: HList, R](f: A => Z)(implicit reverse: Reverse[FS, FSR], m: Map[F, FSR, A => Z, R]): F[R] =
      m(reverse(fs), f)
  }

  sealed trait Map[F[_], FS, FN, R] {
    def apply(fs: FS, f: FN): F[R]
  }

  object Map {
    implicit def m
      [F[_]: Applicative, A, FT <: HList, Z, R]
      (implicit tailAp: Ap[F, FT, F[Z], R])
    : Map[F, HCons[F[A], FT], A => Z, R] = new Map[F, HCons[F[A], FT], A => Z, R] {
      override def apply(fs: HCons[F[A], FT], f: A => Z): F[R] = {
        val M = Applicative[F]
        import M.Ops
        val HCons(fa, ft) = fs
        val fz = fa.map(f)
        tailAp(ft, fz)
      }
    }
  }

  sealed trait Ap[F[_], FS, FF, R] {
    def apply(fs: FS, ff: FF): F[R]
  }

  object Ap {
    implicit def singleAp
      [F[_]: Applicative, A, R]
    : Ap[F, HCons[F[A], HNil], F[A => R], R] = new Ap[F, HCons[F[A], HNil], F[A => R], R] {
      override def apply(fs: HCons[F[A], HNil], ff: F[A => R]): F[R] = {
        val M = Applicative[F]
        import M.Ops
        val HCons(fa, _) = fs
        val fr = fa.ap(ff)
        fr
      }
    }

    implicit def multiAp
      [F[_]: Applicative, A, FT <: HList, Z, R]
      (implicit tailAp: Ap[F, FT, F[Z], R])
    : Ap[F, HCons[F[A], FT], F[A => Z], R] = new Ap[F, HCons[F[A], FT], F[A => Z], R] {
      override def apply(fs: HCons[F[A], FT], ff: F[A => Z]): F[R] = {
        val M = Applicative[F]
        import M.Ops
        val HCons(fa, ft) = fs
        val fz = fa.ap(ff)
        val fr = tailAp(ft, fz)
        fr
      }
    }
  }

  class Laws[F[_]: Applicative](implicit run: Run[F]) {
    private val M = Applicative[F]
    import M.Ops

    def checkIdentity[A](fa: F[A]): Boolean =
      run(fa.ap(M.pure(identity[A](_)))) == run(fa)

    def checkHomomorphism[A, B](a: A, f: A => B): Boolean =
      run(M.pure(a).ap(M.pure(f))) == run(M.pure(f(a)))

    def checkInterchange[A, B](a: A, ff: F[A => B]): Boolean = {
      val $ = (y: A) => (f: A => B) => f(y)
      run(M.pure(a).ap(ff)) == run(ff.ap(M.pure($(a))))
    }

    def checkComposition[A, B, C](u: F[B => C], v: F[A => B], w: F[A]): Boolean = {
      val compose = (f: B => C) => (g: A => B) => f compose g
      run(w.ap(v.ap(u.ap(M.pure(compose))))) == run(w.ap(v).ap(u))
    }
  }

  def laws[F[_]: Applicative: Run]: Laws[F] = new Laws[F]
}
