package faísca

object MapStreamer {
  def apply[A, B](func: A => B, streamer: Streamer[A]): Streamer[B] = streamer match {
    case MapStreamer(srcFunc, srcStreamer) =>
      val newFunc = srcFunc andThen func
      new MapStreamer(newFunc, srcStreamer)
    case _ =>
      new MapStreamer(func, streamer)
  }

  def unapply[T](streamer: Streamer[T]): Option[(Any => T, Streamer[Any])] = {
    if (streamer.isInstanceOf[MapStreamer[_, T]]) {
      val mapStreamer = streamer.asInstanceOf[MapStreamer[Any, T]]
      Some((mapStreamer.func, mapStreamer.streamer))
    } else {
      None
    }
  }
}

class MapStreamer[A, B](private val func: A => B, private val streamer: Streamer[A]) extends Streamer[B] {
  def get(): B = func(streamer.get())
}
