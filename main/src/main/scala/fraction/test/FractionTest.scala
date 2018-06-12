package fraction.test

import TinyInt.FromInt
import fraction.Fraction

object FractionTest extends FractionTestSupport {

  def main(args: Array[String]): Unit = {
    testApply()

    testPlus()
    testMinus()
    testTimes()
    testDiv()

    testUnary_-()
    testAbs()

    testEq()
    testLt()
    testCompareTo()

    testToInt()

    println("OK")
  }

  private def testApply(): Unit =
    testOpenUnaryOperation[(Int, Int), Fraction[TinyInt], (Int, Int)]("apply", constructorInputs)
      { case (intNum, intDen) => Fraction[TinyInt](intNum.toTinyInt, intDen.toTinyInt) }
      { case (intNum, intDen) => (intNum, intDen) }
      { case (fracResult, (intNumResult, intDenResult)) => equivalent(fracResult)(intNumResult, intDenResult) }

  private def testPlus(): Unit =
    testClosedBinaryOperation("+")(_ + _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      val intNum = intNum1 * intDen2 + intNum2 * intDen1
      val intDen = intDen1 * intDen2
      (intNum, intDen)
    }

  private def testMinus(): Unit =
    testClosedBinaryOperation("-")(_ - _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      val intNum = intNum1 * intDen2 - intNum2 * intDen1
      val intDen = intDen1 * intDen2
      (intNum, intDen)
    }

  private def testTimes(): Unit =
    testClosedBinaryOperation("*")(_ * _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      val intNum = intNum1 * intNum2
      val intDen = intDen1 * intDen2
      (intNum, intDen)
    }

  private def testDiv(): Unit = {
    val operands2 = allFractions.filter { _.num != 0 }
    testClosedBinaryOperation("/", operands2 = operands2)(_ / _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      val intNum = intNum1 * intNum2
      val intDen = intDen1 * intDen2
      (intNum, intDen)
    }
  }

  private def testUnary_-(): Unit =
    testClosedUnaryOperation("unary_-")(-_) { case (intNum, intDen) => (-intNum, intDen) }

  private def testAbs(): Unit =
    testClosedUnaryOperation("abs")(_.abs) { case (intNum, intDen) =>
      val intNumResult = if (intNum < 0) -intNum else intNum
      val intDenResult = intDen
      (intNumResult, intDenResult)
    }

  private def testEq(): Unit =
    testOpenOutputBinaryOperation("==")(_ == _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      intNum1 * intDen2 == intNum2 * intDen1
    }

  private def testLt(): Unit =
    testOpenOutputBinaryOperation("<")(_ < _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      intNum1 * intDen2 < intNum2 * intDen1
    }

  private def testCompareTo(): Unit =
    testOpenOutputBinaryOperation("compareTo")(_ compareTo _) { case ((intNum1, intDen1), (intNum2, intDen2)) =>
      (intNum1 * intDen2) compareTo (intNum2 * intDen1)
    }

  private def testToInt(): Unit =
    testOpenUnaryOperation("toInt", allFractions)(_.toInt)
      { (intNum, intDen) => intNum / intDen }
      { (result, intResult) => result == intResult }
}

trait FractionTestSupport {

  private val ValueRange =
    TinyInt.MinValue.toInt to TinyInt.MaxValue.toInt

  protected def constructorInputs =
    for {
      num <- ValueRange.toStream
      den <- ValueRange.toStream
      if den != 0
    } yield (num, den)

  protected def allFractions =
    constructorInputs
      .map { case (num, den) => Fraction[TinyInt](num.toTinyInt, den.toTinyInt) }
      .toStream
      .distinct

  private sealed trait Outcome
  private object Correct extends Outcome
  private object Wrong extends Outcome
  private object Failed extends Outcome
  private object CantFit extends Outcome
  private val AllOutcomes = Seq(Correct, Wrong, Failed, CantFit)

  protected def equivalent(frac: Fraction[TinyInt])(intNum: Int, intDen: Int): Boolean =
    intNum * frac.den == frac.num * intDen

  protected trait ToIntNumAndDen[A] {
    def apply(a: A): (Int, Int)
  }

  protected implicit val intPairToIntNumAndDen: ToIntNumAndDen[(Int, Int)] =
    new ToIntNumAndDen[(Int, Int)] {
      def apply(intPair: (Int, Int)): (Int, Int) = intPair
    }

  protected implicit val fracToIntNumAndDen: ToIntNumAndDen[Fraction[TinyInt]] =
    new ToIntNumAndDen[Fraction[TinyInt]] {
      def apply(frac: Fraction[TinyInt]): (Int, Int) = (frac.num.toInt, frac.den.toInt)
    }

  protected def testOpenUnaryOperation[E, TI, I]
    (operation: String, operands: Seq[E])
    (op: E => TI)(intOp: (Int, Int) => I)
    (resultCheck: (TI, I) => Boolean)
    (implicit toIntNumAndDen: ToIntNumAndDen[E])
  : Unit =
    testUnaryOperation(operation, operands)(op)
      { input => intOp.tupled(toIntNumAndDen(input)) } (fitCheck = _ => true)(resultCheck)

  protected def testClosedUnaryOperation
    (operation: String)
    (op: Fraction[TinyInt] => Fraction[TinyInt])
    (intOp: (Int, Int) => (Int, Int))
  : Unit =
    testUnaryOperation(operation, allFractions)(op)
      { frac => intOp(frac.num.toInt, frac.den.toInt) }
      { case (intNumResult, intDenResult) =>
        val intFracResult = Fraction[Int](intNumResult, intDenResult)
        ValueRange.contains(intFracResult.num)  &&  ValueRange.contains(intFracResult.den)
      }
      { case (fracResult, (intNumResult, intDenResult)) => equivalent(fracResult)(intNumResult, intDenResult) }

  protected def testUnaryOperation[E, TI, I]
    (operationName: String, operands: Seq[E])
    (op: E => TI)(intOp: E => I)
    (fitCheck: I => Boolean)
    (resultCheck: (TI, I) => Boolean)
  : Unit =
    test(operationName, operands)(op)(intOp)(fitCheck)(resultCheck)

  protected def testOpenOutputBinaryOperation[R]
    (operation: String)
    (op: (Fraction[TinyInt], Fraction[TinyInt]) => R)(intOp: ((Int, Int), (Int, Int)) => R)
  : Unit =
    testBinaryOperation(operation, allFractions, allFractions)(op)
      { (firstOperand, secodOperand) =>
        intOp(fracToIntNumAndDen(firstOperand), fracToIntNumAndDen(secodOperand))
      } (_ => true)(_ == _)

  protected def testClosedBinaryOperation(
      operation: String,
      operands1: Seq[Fraction[TinyInt]] = allFractions, operands2:
      Seq[Fraction[TinyInt]] = allFractions
    )
    (op: (Fraction[TinyInt], Fraction[TinyInt]) => Fraction[TinyInt])
    (intOp: ((Int, Int), (Int, Int)) => (Int, Int))
  : Unit =
    testBinaryOperation(operation, operands1, operands2)(op)
      { (frac1, frac2) => intOp(fracToIntNumAndDen(frac1), fracToIntNumAndDen(frac2)) }
      { case (intNumResult, intDenResult) =>
        val intFracResult = Fraction[Int](intNumResult, intDenResult)
        ValueRange.contains(intFracResult.num)  &&  ValueRange.contains(intFracResult.den)
      }
      { case (fracResult, (intNumResult, intDenResult)) => intNumResult * fracResult.den == fracResult.num * intDenResult }

  protected def testBinaryOperation[E, TI, I]
    (operationName: String, operands1: Seq[E], operands2: Seq[E])
    (op: (E, E) => TI)(intOp: (E, E) => I)
    (fitCheck: I => Boolean)
    (resultCheck: (TI, I) => Boolean)
  : Unit = {
    val operandPairs =
      for {
        operand1 <- operands1
        operand2 <- operands2
      } yield (operand1, operand2)
    test(operationName, operandPairs)(op.tupled)(intOp.tupled)(fitCheck)(resultCheck)
  }

  protected def test[E, TI, I]
    (operationName: String, operands: Seq[E])
    (op: E => TI)(intOp: E => I)
    (fitCheck: I => Boolean)
    (resultCheck: (TI, I) => Boolean)
  : Unit = {
    val counters0 = AllOutcomes.map(_ -> 0).toMap
    val counters = operands.foldLeft(counters0) { (counters, operand) =>
      val intResult = intOp(operand)
      val outcome =
        if (fitCheck(intResult)) {
          val result = try Some(op(operand)) catch { case _: IllegalArgumentException => None }
          result match {
            case Some(result) => if (resultCheck(result, intResult)) Correct else Wrong
            case None => Failed
          }
        } else {
          CantFit
        }

      val counterValue = counters(outcome)
      val newCounterValue = counterValue + 1
      counters + (outcome -> newCounterValue)
    }

    val total = counters.values.sum

    println(s"${operationName}:")
    println(s"  Out of ${total} possible inputs:")
    showCounterIfNotZero(counters(Correct), total, "have a " + colored("32;1")("correct result"))
    showCounterIfNotZero(counters(Wrong), total, "have a " + colored("31;1")("wrong result"))
    showCounterIfNotZero(counters(Failed), total, colored("41;1")("failed with an exception"))
    showCounterIfNotZero(counters(CantFit), total, "have a " + colored("33;1")("result which can't fit"))
  }

  private def colored(code: String)(text: String): String = ansiCode(code) + text + ansiCode("0")

  private def ansiCode(code: String): String = "\u001b[" + code + "m"

  private def showCounterIfNotZero(counter: Int, total: Int, description: String): Unit =
    if (counter > 0) {
      val percent = 100.0 * counter / total
      println(f"    * ${counter}%5d (${percent}%4.1f%%) ${description}%s")
    }
}
