package transposition.test

import java.util.{ArrayList, Collections, List => JavaList}
import transposition.{Empty, HomoSeqCons, HomoSeqTag, HomoSeqUncons}

object JList {

  private type Tag = HomoSeqTag[JavaList]

  implicit def jlistEmpty[
    A
  ]: Empty[
    Tag, // Tag
    JavaList[A], // S
  ] =
    () => Collections.emptyList()

  implicit def jlistCons[
    A
  ]: HomoSeqCons[
    A, // H
    JavaList[A], // T
    Tag, // Tag
  ] =
    (h: A, t: JavaList[A]) => {
      val list = new ArrayList[A]()
      list.add(h)
      list.addAll(t)
      list
    }

  implicit def jlistUncons[
    A
  ]: HomoSeqUncons[
    JavaList[A], // S
    Tag, // Tag
    A, // A
  ] =
    (s: JavaList[A]) =>
      if (s.isEmpty) {
        None
      } else {
        val head = s.get(0)
        val tail = s.subList(1, s.size())
        Some((head, tail))
      }
}
