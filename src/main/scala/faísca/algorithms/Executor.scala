package faísca.algorithms

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.mutable

trait Streamer[A] {
  protected def nextImpl(): Option[A]
  private var finalized = false

  final def next(): Option[A] =
    if (finalized) None
    else nextImpl() orElse {
      finalized = true
      None
    }
}

object Streamer {
  def empty[A]: Streamer[A] = ???
  def apply[A](as: GenTraversable[A]): Streamer[A] = ???
}

class MapStreamer[A, B](f: A => B, previous: Streamer[A]) extends Streamer[B] {
  protected def nextImpl(): Option[B] =
    previous.next() match {
      case Some(a) => Some(f(a))
      case None => None
    }
}

class FlatMapStreamer[A, B](f: A => GenTraversable[B], previous: Streamer[A]) extends Streamer[B] {
  private var pending: Streamer[B] = Streamer.empty

  @tailrec
  protected final def nextImpl(): Option[B] =
    pending.next() match {
      case n@Some(b) => n
      case None => {
        previous.next() match {
          case Some(a) => {
            val bs = f(a)
            pending = Streamer(bs)
            nextImpl()
          }
          case None => None
        }
      }
    }
}

class FilterStreamer[A](f: A => Boolean, previous: Streamer[A]) extends Streamer[A] {
  @tailrec
  protected final def nextImpl(): Option[A] =
    previous.next() match {
      case n@Some(a) => if (f(a)) n else nextImpl()
      case None => None
    }
}

class RedistributeStreamer[A](previous: Streamer[A]) extends Streamer[A] {
  protected def nextImpl(): Option[A] = ???
}

class DistinctStreamer[A](previous: Streamer[A]) extends Streamer[A] {
  private val redistributor = new RedistributeStreamer(previous)
  private val uniqueValues: mutable.Set[A] = mutable.HashSet.empty

  @tailrec
  protected final def nextImpl(): Option[A] =
    redistributor.next() match {
      case n@Some(a) => if (uniqueValues.add(a)) n else nextImpl()
      case None => {
        uniqueValues.clear()
        None
      }
    }
}
