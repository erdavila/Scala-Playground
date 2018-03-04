package guideToShapeless

import shapeless.Witness
import shapeless.syntax.singleton._
import shapeless.test.illTyped
import shapeless.test.typed

object LiteralTypes {

  def main(args: Array[String]): Unit = {
    values()
    types()
  }

  private def values(): Unit = {
    val variable = 123
    val otherVariable = 123

    val literalWithType = 123.narrow
    val variableWithType = variable.narrow
    val otherVariableWithType = otherVariable.narrow

    val literalWitness = 123.witness
    val variableWitness = variable.witness

    typed[Int](123)
    typed[Int](variable)
    typed[Int](literalWithType)
    typed[Int](variableWithType)
    typed[Int](literalWitness.value)
    typed[Int](variableWitness.value)

              typed[literalWitness.T](123)
    illTyped("typed[literalWitness.T](456)")
    illTyped("typed[literalWitness.T](variable)")
              typed[literalWitness.T](literalWithType)
    illTyped("typed[literalWitness.T](variableWithType)")
              typed[literalWitness.T](literalWitness.value)
    illTyped("typed[literalWitness.T](variableWitness.value)")

    illTyped("typed[variableWitness.T](123)")
              typed[variableWitness.T](variable)
    illTyped("typed[variableWitness.T](otherVariable)")
    illTyped("typed[variableWitness.T](literalWithType)")
              typed[variableWitness.T](variableWithType)
    illTyped("typed[variableWitness.T](otherVariableWithType)")
    illTyped("typed[variableWitness.T](literalWitness.value)")
              typed[variableWitness.T](variableWitness.value)
  }

  private def types(): Unit = {
    val narrowValue = 123.narrow
    val narrowTypeHolder = TypeHolder.of(narrowValue)
    type NarrowType = narrowTypeHolder.T

    val witness = Witness(123)

    sameTyped(narrowValue, witness.value)
    sameTypes[NarrowType, witness.T]()

    assertSameValueAndType(123)(getValueFromNarrowType[witness.T])
  }

  private trait TypeHolder {
    type T
  }

  private object TypeHolder {
    type Aux[U] = TypeHolder { type T = U }

    def of[U](value: U): Aux[U] =
      new TypeHolder {
        type T = U
      }
  }

  private def getValueFromNarrowType[T](implicit witness: Witness.Aux[T]) =
    witness.value
}
