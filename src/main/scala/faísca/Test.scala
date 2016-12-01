package faísca

object Test extends App {
  val staticData = new StaticStreamer(7)
  val staticDS = new Dataset(staticData)
  assert(staticDS.collect() == 7)

  val map1DS = staticDS map { _ / 2.0 }
  assert(map1DS.collect() == 3.5)

  val map2DS = map1DS map { _.toString() }
  assert(map2DS.collect() == "3.5")

  println("OK")
}
