package faísca.algorithms

import scala.annotation.tailrec
import scala.collection.GenTraversable
import scala.collection.mutable

trait Streamer[A] {
  protected def fetchNext(): Option[A]
  private var finalized = false

  final def next(): Option[A] =
    if (finalized) None
    else fetchNext() orElse {
      finalized = true
      None
    }

  def length(): Long
}

object Streamer {
  def empty[A]: Streamer[A] = ???
  def apply[A](as: GenTraversable[A]): Streamer[A] = ???
}

class MapStreamer[A, B](f: A => B, previous: Streamer[A]) extends Streamer[B] {
  protected def fetchNext(): Option[B] =
    previous.next() match {
      case Some(a) => Some(f(a))
      case None => None
    }

  def length() = previous.length()
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

  def length() = remainingItemsInStreamer()

  @tailrec
  private def remainingItemsInStreamer(): Long =
    previous.next() match {
      case Some(value) => {
        val replacement = f(value)
        val replacementLen = replacement.foldLeft(0) { (n, _) => n + 1 }
        replacementLen + remainingItemsInStreamer()
      }
      case None => 0
    }
}

class FilterStreamer[A](f: A => Boolean, previous: Streamer[A]) extends Streamer[A] {
  @tailrec
  protected final def fetchNext(): Option[A] =
    previous.next() match {
      case n@Some(a) => if (f(a)) n else fetchNext()
      case None => None
    }

  def length() = remainingItemsInStreamer()

  @tailrec
  private def remainingItemsInStreamer(): Long =
    previous.next() match {
      case Some(value) => {
        val inc = if (f(value)) 1 else 0
        inc + remainingItemsInStreamer()
      }
      case None => 0
    }
}

sealed trait ChannelSignal
case object NoDataAvailable extends ChannelSignal
case object ChannelClosed extends ChannelSignal

trait InputChannel[A] {
  def get(): Either[ChannelSignal, A]
}

class InputChannelsAggregator[A](inputChannels: InputChannel[A]*) extends InputChannel[A] {
  private val channels = mutable.ArrayBuffer(inputChannels: _*)
  private var nextChannelNumber = 0

  @tailrec
  final def get(): Either[ChannelSignal, A] =
    if (channels.isEmpty) Left(ChannelClosed)
    else channels(nextChannelNumber).get() match {
      case v@Right(_) => {
        incrementNextChannelNumber()
        v
      }
      case Left(NoDataAvailable) => {
        incrementNextChannelNumber()
        get()
      }
      case Left(ChannelClosed) => {
        channels.remove(nextChannelNumber)
        get()
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

class OutputChannelsMultiplexer[A](outputChannels: Option[OutputChannel[A]]*) {
  def put(value: A): Boolean = {
    val destinationExecutor = value.hashCode() % outputChannels.length
    outputChannels(destinationExecutor) match {
      case Some(channel) => {
        channel.put(value)
        true
      }
      case None => false
    }
  }

  def close(): Unit = outputChannels.foreach { _.foreach { _.close() } }
}

class StreamerInputChannelAdapter[A](streamer: Streamer[A]) extends InputChannel[A] {
  def get(): Either[ChannelSignal, A] =
    streamer.next() match {
      case Some(value) => Right(value)
      case None => Left(ChannelClosed)
    }
}

class RedistributeStreamer[A](previous: Streamer[A]) extends Streamer[A] {
  private val inputChannels = new InputChannelsAggregator[A](new StreamerInputChannelAdapter(previous), ???)
  private val outputChannels = new OutputChannelsMultiplexer[A](???)

  @tailrec
  final protected def fetchNext(): Option[A] =
    inputChannels.get() match {
      case Right(value) => {
        val valueWasMultiplexed = outputChannels.put(value)
        if (valueWasMultiplexed) fetchNext() else Some(value)
      }
      case Left(NoDataAvailable) => fetchNext()
      case Left(ChannelClosed) => {
        outputChannels.close()
        None
      }
    }

  def length() = remainingItemsInStreamer()

  @tailrec
  private def remainingItemsInStreamer(): Long =
    inputChannels.get() match {
      case Right(value) => {
        val valueWasMultiplexed = outputChannels.put(value)
        (if (valueWasMultiplexed) 0 else 1) + remainingItemsInStreamer()
      }
      case Left(NoDataAvailable) => remainingItemsInStreamer()
      case Left(ChannelClosed) => {
        outputChannels.close()
        0
      }
    }
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
