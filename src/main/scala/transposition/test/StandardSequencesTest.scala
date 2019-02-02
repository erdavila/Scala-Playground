package transposition.test

import shapeless.{::, HNil}
import transposition.test.DSL.transposed

object StandardSequencesTest {

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
    type Input = Vector[Vector[Int]]
    type Output = Vector[Vector[Int]]

    transposed[Input](
      Vector(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9),
      )
    ).must.be[Output](
      Vector(
        Vector(1, 4, 7),
        Vector(2, 5, 8),
        Vector(3, 6, 9),
      )
    )

    transposed[Input](Vector(Vector.empty, Vector.empty, Vector.empty))
      .must.be[Output](Vector.empty)

    transposed[Input](Vector.empty)
      .must.be[Output](Vector.empty)
  }

  private def testHomoSeqOfNonEmptyHeteroSeqs(): Unit = {
    type Input = Vector[Int :: String :: Boolean :: HNil]
    type Output = Vector[Int] :: Vector[String] :: Vector[Boolean] :: HNil

    transposed[Input](
      Vector(
        1 :: "A" ::  true :: HNil,
        2 :: "B" :: false :: HNil,
        3 :: "C" ::  true :: HNil,
      )
    ).must.be[Output](
      Vector(1, 2, 3) ::
      Vector("A", "B", "C") ::
      Vector(true, false, true) ::
      HNil
    )

    transposed[Input](
      Vector.empty
    ).must.be[Output](
      Vector.empty[Int] ::
      Vector.empty[String] ::
      Vector.empty[Boolean] ::
      HNil
    )
  }

  private def testHomoSeqOfEmptyHeteroSeqs(): Unit = {
    type Input = Vector[HNil]
    type Output = HNil

    transposed[Input](Vector(HNil, HNil, HNil))
      .must.be[Output](HNil)

    transposed[Input](Vector.empty)
      .must.be[Output](HNil)
  }

  private def testHeteroSeqOfHomoSeqs(): Unit = {
    type Input = Vector[Int] :: Vector[String] :: Vector[Boolean] :: HNil
    type Output = Vector[Int :: String :: Boolean :: HNil]

    transposed[Input](
      Vector(1, 2, 3) ::
      Vector("A", "B", "C") ::
      Vector(true, false, true) ::
      HNil
    ).must.be[Output](
      Vector(
        1 :: "A" :: true  :: HNil,
        2 :: "B" :: false :: HNil,
        3 :: "C" :: true  :: HNil,
      )
    )

    transposed[Input](
      Vector.empty[Int] ::
      Vector.empty[String] ::
      Vector.empty[Boolean] ::
      HNil
    ).must.be[Output](
      Vector.empty[Int :: String :: Boolean :: HNil]
    )
  }

  private def testHeteroSeqOfNonEmptyHeteroSeqs(): Unit = {
    type Input =
      (Int     :: String  :: Boolean :: HNil) ::
      (Boolean :: Int     :: String  :: HNil) ::
      (String  :: Boolean :: Int     :: HNil) ::
      HNil
    type Output =
      (Int     :: Boolean :: String  :: HNil) ::
      (String  :: Int     :: Boolean :: HNil) ::
      (Boolean :: String  :: Int     :: HNil) ::
      HNil

    transposed[Input](
      (1     :: "A"  :: true :: HNil) ::
      (false :: 2    :: "B"  :: HNil) ::
      ("C"   :: true :: 3    :: HNil) ::
      HNil
    ).must.be[Output](
      (1    :: false :: "C"  :: HNil) ::
      ("A"  :: 2     :: true :: HNil) ::
      (true :: "B"   :: 3    :: HNil) ::
      HNil
    )
  }

  private def testHeteroSeqOfEmptyHeteroSeqs(): Unit = {
    type Input = HNil :: HNil :: HNil :: HNil
    type Output = HNil

    transposed[Input](HNil :: HNil :: HNil :: HNil)
      .must.be[Output](HNil)
  }
}
