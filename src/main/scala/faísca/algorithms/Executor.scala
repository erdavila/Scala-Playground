package faísca.algorithms

import scala.annotation.tailrec
import scala.collection.{GenTraversable, mutable}

trait Streamer[A] {
  protected def fetchNext(): Option[A]

  private var finalized = false

  final def next(): Option[A] =
    if (finalized) None
    else fetchNext() orElse {
      finalized = true
      None
    }
}

object Streamer {
  def empty[A]: Streamer[A] = new Streamer[A] {
    protected def fetchNext(): Option[A] = None
  }

  def apply[A](as: Iterator[A]): Streamer[A] = new Streamer[A] {
    protected def fetchNext(): Option[A] =
      if (as.hasNext) Some(as.next())
      else None
  }

  def apply[A](as: GenTraversable[A]): Streamer[A] = apply(as.toIterator)

  def apply[A](channel: InputChannel[A]): Streamer[A] = new Streamer[A] {
    protected def fetchNext(): Option[A] = channel.get() match {
      case Right(value) => Some(value)
      case Left(NoDataAvailable) => fetchNext()
      case Left(ChannelClosed) => None
    }
  }
}

class MapStreamer[A, B](f: A => B, previous: Streamer[A]) extends Streamer[B] {
  protected def fetchNext(): Option[B] =
    previous.next() match {
      case Some(a) => Some(f(a))
      case None => None
    }
}

class FlatMapStreamer[A, B](f: A => GenTraversable[B], previous: Streamer[A]) extends Streamer[B] {
  private var pending: Streamer[B] = Streamer.empty

  @tailrec
  protected final def fetchNext(): Option[B] =
    pending.next() match {
      case n@Some(b) => n
      case None => {
        previous.next() match {
          case Some(a) => {
            val bs = f(a)
            pending = Streamer(bs)
            fetchNext()
          }
          case None => None
        }
      }
    }
}

class FilterStreamer[A](f: A => Boolean, previous: Streamer[A]) extends Streamer[A] {
  @tailrec
  protected final def fetchNext(): Option[A] =
    previous.next() match {
      case n@Some(a) => if (f(a)) n else fetchNext()
      case None => None
    }
}

sealed trait ChannelSignal
case object NoDataAvailable extends ChannelSignal
case object ChannelClosed extends ChannelSignal

trait InputChannel[A] {
  def get(): Either[ChannelSignal, A] =
    if (closed) Left(ChannelClosed)
    else fetch() match {
      case cc@Left(ChannelClosed) => {
        closed = true
        cc
      }
      case e => e
    }

  private var closed = false

  protected def fetch(): Either[ChannelSignal, A]
}

object InputChannel {
  def apply[A](streamer: Streamer[A]): InputChannel[A] = new InputChannel[A] {
    def fetch(): Either[ChannelSignal, A] = streamer.next() match {
      case Some(value) => Right(value)
      case None => Left(ChannelClosed)
    }
  }
}

class InputChannelsAggregator[A](inputChannels: InputChannel[A]*) extends InputChannel[A] {
  private val channels = mutable.ArrayBuffer(inputChannels: _*)
  private var nextChannelNumber = 0

  @tailrec
  final def fetch(): Either[ChannelSignal, A] =
    if (channels.isEmpty) Left(ChannelClosed)
    else channels(nextChannelNumber).get() match {
      case v@Right(_) => {
        incrementNextChannelNumber()
        v
      }
      case Left(NoDataAvailable) => {
        incrementNextChannelNumber()
        fetch()
      }
      case Left(ChannelClosed) => {
        channels.remove(nextChannelNumber)
        fetch()
      }
    }

  private def incrementNextChannelNumber(): Unit = {
    nextChannelNumber += 1
    nextChannelNumber %= channels.length
  }
}

trait OutputChannel[A] {
  def put(a: A): Unit
  def close(): Unit
}

class MultiplexerStreamer[A](source: Streamer[A], outputChannels: Seq[Option[OutputChannel[A]]]) extends Streamer[A] {
  @tailrec
  final protected def fetchNext(): Option[A] = source.next() match {
    case o@Some(value) => {
      val destinationExecutorNumber = value.hashCode() % outputChannels.length
      outputChannels(destinationExecutorNumber) match {
        case Some(channel) => {
          channel.put(value)
          fetchNext()
        }
        case None => o
      }
    }
    case None => {
      outputChannels.foreach { _.foreach { _.close() } }
      None
    }
  }
}

class RedistributeStreamer[A](previous: Streamer[A]) extends Streamer[A] {
  private val executorNumber = ??? : Int
  private val executorsOutputChannels = ??? : Seq[OutputChannel[A]]
  private val otherExecutorsOutputChannels = executorsOutputChannels.zipWithIndex map { case (ch, i) => if (ch == executorNumber) None else Some(ch) }

  private val muxStreamer = new MultiplexerStreamer(previous, otherExecutorsOutputChannels)
  private val muxChannel = InputChannel(muxStreamer)

  private val otherExecutorsInputChannels = ??? : Seq[InputChannel[A]]
  private val aggregatorChannel = new InputChannelsAggregator((muxChannel +: otherExecutorsInputChannels): _*)
  private val aggregatorStreamer = Streamer(aggregatorChannel)

  protected final def fetchNext(): Option[A] = aggregatorStreamer.next()
}

class DistinctStreamer[A](previous: Streamer[A]) extends Streamer[A] {
  private val redistributor = new RedistributeStreamer(previous)
  private val uniqueValues: mutable.Set[A] = mutable.HashSet.empty

  @tailrec
  protected final def fetchNext(): Option[A] =
    redistributor.next() match {
      case n@Some(a) => if (uniqueValues.add(a)) n else fetchNext()
      case None => {
        uniqueValues.clear()
        None
      }
    }
}
