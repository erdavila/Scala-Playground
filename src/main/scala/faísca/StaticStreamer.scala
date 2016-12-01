package faísca

class StaticStreamer[T](value: T) extends Streamer[T] {
  def get(): T = value
}
