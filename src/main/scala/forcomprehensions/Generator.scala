package forcomprehensions

import scala.util.Random

object Generator extends App {
  val FirstVarName = 'a'
  var nextVarName = FirstVarName

  val generators = Seq[() => String](
    generateIteration _,
    generatePatternIteration _,
    generateCondition _,
    generateAuxVariable _
  )

  println("for {")
  println("  " + generateFirstItem())
  (2 to 20) foreach { _ => println("  " + generateItem()) }
  println("} yield expression" + usedVarsParameters(nextVarName))

  def generateFirstItem() = Random.nextInt(2) match {
    case 0 => generateIteration()
    case 1 => generatePatternIteration()
  }

  def generateItem() = {
    val idx = Random.nextInt(generators.length)
    generators(idx)()
  }

  def generateIteration() = {
    val varName = consumeVarName()
    val usedVars = usedVarsParameters(varName)
    varName + " <- monadFor(\"" + varName + "\")" + usedVars
  }

  def generatePatternIteration() = {
    val varName = consumeVarName()
    "Pattern(" + varName + ")" + " <- monadFor(\"" + varName + "\")" + usedVarsParameters(varName)
  }

  def generateCondition() = "if condition" + usedVarsParameters(nextVarName)

  def generateAuxVariable() = {
    val varName = consumeVarName()
    val usedVars = usedVarsParameters(varName)
    varName + " = valueFor(\"" + varName + "\")" + usedVars
  }

  def consumeVarName() = {
    val varName = nextVarName
    nextVarName = (nextVarName + 1).toChar
    varName
  }

  def usedVarsParameters(limitVarName: Char) = "(" + (FirstVarName until limitVarName mkString ", ") + ")"
}
