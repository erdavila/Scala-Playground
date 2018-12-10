package transposition.test

import java.util.Arrays.{asList => javaList}
import java.util.{List => JavaList}

import shapeless.{::, HNil}
import transposition.test.DSL.transposed
import transposition.test.TList._
import transposition.test.JList._

object MixedSequencesTest {

  def main(args: Array[String]): Unit = {
    homoSeqs()
    heteroSeqs()

    println("OK")
  }

  private def homoSeqs(): Unit = {
    type Input = Vector[JavaList[Int]]
    type Output = JavaList[Vector[Int]]

    transposed[Input](
      Vector(
        javaList(1, 2, 3),
        javaList(4, 5, 6),
        javaList(7, 8, 9),
      )
    ).must.be[Output](
      javaList(
        Vector(1, 4, 7),
        Vector(2, 5, 8),
        Vector(3, 6, 9),
      )
    )

    transposed[Input](Vector(javaList(), javaList(), javaList()))
      .must.be[Output](javaList())

    transposed[Input](Vector.empty)
      .must.be[Output](javaList())
  }

  private def heteroSeqs(): Unit = {
    type Input =
      (Int     :: String  :: Boolean :: HNil,
      (Boolean :: Int     :: String  :: HNil,
      (String  :: Boolean :: Int     :: HNil,
      Unit)))
    type Output =
      (Int    , (Boolean, (String , Unit))) ::
      (String , (Int    , (Boolean, Unit))) ::
      (Boolean, (String , (Int    , Unit))) ::
      HNil

    transposed[Input](
      (1     :: "A"  :: true :: HNil,
      (false :: 2    :: "B"  :: HNil,
      ("C"   :: true :: 3    :: HNil,
      ())))
    ).must.be[Output](
      (1   , (false, ("C" , ()))) ::
      ("A" , (2    , (true, ()))) ::
      (true, ("B"  , (3   , ()))) ::
      HNil
    )
  }
}
