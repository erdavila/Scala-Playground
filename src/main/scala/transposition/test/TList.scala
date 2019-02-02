package transposition.test

import transposition.{Cons, Empty, HeteroSeqTag, IsStaticEmpty, Uncons}

final class TList[S]

object TList {

  private type Tag = HeteroSeqTag[Unit]

  implicit val tuple0TList: TList[Unit] =
    new TList[Unit]

  implicit def tuple2TList[H, T: TList]: TList[(H, T)] =
    new TList[(H, T)]

  implicit val tuple0IsStaticEmpty: IsStaticEmpty[
    Unit,
    Tag,
   ] =
    new IsStaticEmpty[Unit, Tag]

  implicit val tlistEmpty: Empty[
    Tag, // Tag
    Unit, // S
  ] =
    () => ()

  implicit def tlistCons[
    H, T: TList
  ]: Cons[
    H, // H
    T, // T
    Tag, // Tag
    (H, T), // S
  ] =
    (h: H, t: T) => (h, t)

  implicit def tlistUncons[
    H, T: TList
  ]: Uncons[
    (H, T), // S
    Tag, // Tag
    H, // H
    T, // T
  ] =
    (s: (H, T)) => Some(s)
}
