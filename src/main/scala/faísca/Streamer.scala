package faísca

trait Streamer[T] {
  def get(): T
}
