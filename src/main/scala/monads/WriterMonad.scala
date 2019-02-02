package monads

import scala.language.reflectiveCalls

object WriterMonad {
  type Writer[W, A] = (W, A)

  object Writer {
    def apply[W, A](w: W, a: A): Writer[W, A] = (w, a)
    def unapply[W, A](writer: Writer[W, A]): Option[(W, A)] = Some(writer)
  }

  implicit def writerMonad2Impl[W](implicit monoid: Monoid[W]): Monad2Impl[Writer, W] = new Monad2Impl[Writer, W] {
    override def map[A, B](fa: Writer[W, A])(f: A => B): Writer[W, B] = {
      val Writer(w, a) = fa
      val b = f(a)
      Writer(w, b)
    }

    override def pure[A](a: A): Writer[W, A] =
      Writer(monoid.zero, a)

    override def ap[A, B](fa: Writer[W, A])(ff: Writer[W, A => B]): Writer[W, B] = {
      val Writer(wa, a) = fa
      val Writer(wf, f) = ff
      val b = f(a)
      val w = monoid.append(wf, wa)
      Writer(w, b)
    }

    override def flatMap[A, B](fa: Writer[W, A])(f: A => Writer[W, B]): Writer[W, B] = {
      val Writer(wa, a) = fa
      val Writer(wb, b) = f(a)
      val w = monoid.append(wa, wb)
      Writer(w, b)
    }
  }
}
