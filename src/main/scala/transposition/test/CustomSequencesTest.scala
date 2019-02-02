package transposition.test

import java.util.Arrays.{asList => javaList}
import java.util.{List => JavaList}
import transposition.test.DSL.transposed
import transposition.test.JList._
import transposition.test.TList._

object CustomSequencesTest {

  def main(args: Array[String]): Unit = {
    testHomoSeqOfHomoSeqs()
    testHomoSeqOfNonEmptyHeteroSeqs()
    testHomoSeqOfEmptyHeteroSeqs()

    testHeteroSeqOfHomoSeqs()
    testHeteroSeqOfNonEmptyHeteroSeqs()
    testHeteroSeqOfEmptyHeteroSeqs()

    println("OK")
  }

  private def testHomoSeqOfHomoSeqs(): Unit = {
    type Input = JavaList[JavaList[Int]]
    type Output = JavaList[JavaList[Int]]

    transposed[Input](
      javaList(
        javaList(1, 2, 3),
        javaList(4, 5, 6),
        javaList(7, 8, 9),
      )
    ).must.be[Output](
      javaList(
        javaList(1, 4, 7),
        javaList(2, 5, 8),
        javaList(3, 6, 9),
      )
    )

    transposed[Input](javaList(javaList(), javaList(), javaList()))
      .must.be[Output](javaList())

    transposed[Input](javaList())
      .must.be[Output](javaList())
  }

  private def testHomoSeqOfNonEmptyHeteroSeqs(): Unit = {
    type Input = Vector[(Int, (String, (Boolean, Unit)))]
    type Output = (Vector[Int], (Vector[String], (Vector[Boolean], Unit)))

    transposed[Input](
      Vector(
        (1, ("A", (true,  ()))),
        (2, ("B", (false, ()))),
        (3, ("C", (true,  ()))),
      )
    ).must.be[Output](
      (Vector(1, 2, 3),
      (Vector("A", "B", "C"),
      (Vector(true, false, true),
      ())))
    )

    transposed[Input](
      Vector.empty
    ).must.be[Output](
      (Vector.empty[Int],
      (Vector.empty[String],
      (Vector.empty[Boolean],
      ())))
    )
  }

  private def testHomoSeqOfEmptyHeteroSeqs(): Unit = {
    type Input = Vector[Unit]
    type Output = Unit

    transposed[Input](Vector((), (), ()))
      .must.be[Output](())

    transposed[Input](Vector.empty)
      .must.be[Output](())
  }

  private def testHeteroSeqOfHomoSeqs(): Unit = {
    type Input = (JavaList[Int], (JavaList[String], (JavaList[Boolean], Unit)))
    type Output = JavaList[(Int, (String, (Boolean, Unit)))]

    transposed[Input](
      (javaList(1, 2, 3),
      (javaList("A", "B", "C"),
      (javaList(true, false, true),
      ())))
    ).must.be[Output](
      javaList(
        (1, ("A", (true , ()))),
        (2, ("B", (false, ()))),
        (3, ("C", (true , ()))),
      )
    )

    transposed[Input](
      (javaList(),
      (javaList(),
      (javaList(),
      ())))
    ).must.be[Output](
      javaList()
    )
  }

  def testHeteroSeqOfNonEmptyHeteroSeqs(): Unit = {
    type Input =
      ((Int,     (String,  (Boolean, Unit))),
      ((Boolean, (Int,     (String,  Unit))),
      ((String,  (Boolean, (Int,     Unit))),
      Unit)))
    type Output =
      ((Int,     (Boolean, (String,  Unit))),
      ((String,  (Int,     (Boolean, Unit))),
      ((Boolean, (String,  (Int,     Unit))),
      Unit)))

    transposed[Input](
      ((1,     ("A",  (true, ()))),
      ((false, (2,    ("B",  ()))),
      (("C",   (true, (3,    ()))),
      ())))
    ).must.be[Output](
      ((1,    (false, ("C",  ()))),
      (("A",  (2,     (true, ()))),
      ((true, ("B",   (3,    ()))),
      ())))
    )
  }

  private def testHeteroSeqOfEmptyHeteroSeqs(): Unit = {
    type Input = (Unit, (Unit, (Unit, Unit)))
    type Output = Unit

    transposed[Input](((), ((), ((), ()))))
      .must.be[Output](())
  }
}
