package faísca

class Dataset[T](streamer: Streamer[T]) {
  def collect(): T = streamer.get()

  def map[U](func: T => U): Dataset[U] = new Dataset(MapStreamer(func, streamer))
}
