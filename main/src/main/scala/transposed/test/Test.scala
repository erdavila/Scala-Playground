package transposed.test

import shapeless.::
import shapeless.HNil
import transposed.test.DSL.transposed

object Test {

  def main(args: Array[String]): Unit = {
    testListOfLists()
    testListOfVectors()
    testListOfHLists()
    testListOfHNils()

    testVectorOfLists()
    testVectorOfVectors()
    testVectorOfHLists()
    testVectorOfHNils()

    testHListOfLists()
    testHListOfVectors()
    testHListOfHLists()
    testHListOfHNils()

    println("OK")
  }

  private def testListOfLists(): Unit = {
    type Input = List[List[Int]]
    type Output = List[List[Int]]

    transposed[Input](
      List(
        List(1, 2, 3),
        List(4, 5, 6),
        List(7, 8, 9),
      )
    ).must.be[Output](
      List(
        List(1, 4, 7),
        List(2, 5, 8),
        List(3, 6, 9),
      )
    )

    transposed[Input](List(Nil, Nil, Nil))
      .must.be[Output](Nil)

    transposed[Input](Nil)
      .must.be[Output](Nil)
  }

  private def testListOfVectors(): Unit = {
    type Input = List[Vector[Int]]
    type Output = Vector[List[Int]]

    transposed[Input](
      List(
        Vector(1, 2, 3),
        Vector(4, 5, 6),
        Vector(7, 8, 9),
      )
    ).must.be[Output](
      Vector(
        List(1, 4, 7),
        List(2, 5, 8),
        List(3, 6, 9),
      )
    )

    transposed[Input](
      List(
        Vector.empty,
        Vector.empty,
        Vector.empty,
      )
    ).must.be[Output](Vector.empty)

    transposed[Input](Nil)
      .must.be[Output](Vector.empty)
  }

  private def testListOfHLists(): Unit = {
    type Input = List[Int :: String :: Boolean :: HNil]
    type Output = List[Int] :: List[String] :: List[Boolean] :: HNil

    transposed[Input](
      List(
        1 :: "A" ::  true :: HNil,
        2 :: "B" :: false :: HNil,
        3 :: "C" ::  true :: HNil,
      )
    ).must.be[Output](
      List(1, 2, 3) ::
      List("A", "B", "C") ::
      List(true, false, true) ::
      HNil
    )

    transposed[Input](Nil)
      .must.be[Output](Nil :: Nil :: Nil :: HNil)
  }

  private def testListOfHNils(): Unit = {
    type Input = List[HNil]
    type Output = HNil

    transposed[Input](List(HNil, HNil, HNil))
      .must.be[Output](HNil)

    transposed[Input](Nil)
      .must.be[Output](HNil)
  }

  private def testVectorOfLists(): Unit = {
    type Input = Vector[List[Int]]
    type Output = List[Vector[Int]]

    transposed[Input](
      Vector(
        List(1, 2, 3),
        List(4, 5, 6),
        List(7, 8, 9),
      )
    ).must.be[Output](
      List(
        Vector(1, 4, 7),
        Vector(2, 5, 8),
        Vector(3, 6, 9),
      )
    )

    transposed[Input](Vector(Nil))
      .must.be[Output](Nil)

    transposed[Input](Vector.empty)
      .must.be[Output](Nil)
  }

  private def testVectorOfVectors(): Unit = {
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

  private def testVectorOfHLists(): Unit = {
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

  private def testVectorOfHNils(): Unit = {
    type Input = Vector[HNil]
    type Output = HNil

    transposed[Input](Vector(HNil, HNil, HNil))
      .must.be[Output](HNil)

    transposed[Input](Vector.empty)
      .must.be[Output](HNil)
  }

  private def testHListOfLists(): Unit = {
    type Input = List[Int] :: List[String] :: List[Boolean] :: HNil
    type Output = List[Int :: String :: Boolean :: HNil]

    transposed[Input](
      List(1, 2, 3) ::
      List("A", "B", "C") ::
      List(true, false, true) ::
      HNil
    ).must.be[Output](
      List(
        1 :: "A" :: true  :: HNil,
        2 :: "B" :: false :: HNil,
        3 :: "C" :: true  :: HNil,
      )
    )

    transposed[Input](Nil :: Nil :: Nil :: HNil)
      .must.be[Output](Nil)
  }

  private def testHListOfVectors(): Unit = {
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

  private def testHListOfHLists(): Unit = {
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

  private def testHListOfHNils(): Unit = {
    type Input = HNil :: HNil :: HNil :: HNil
    type Output = HNil

    transposed[Input](HNil :: HNil :: HNil :: HNil)
      .must.be[Output](HNil)
  }
}
