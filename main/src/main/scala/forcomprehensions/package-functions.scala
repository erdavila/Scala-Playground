package object forcomprehensions {

  def monadFor(varName: String)(usedVars: Variable*): Monad[Variable] = MonadFor(SimpleVar(varName), usedVars)

  def valueFor(varName: String)(firstUsedVar: Variable, restUsedVars: Variable*): Variable =
    AuxVar(varName, firstUsedVar +: restUsedVars)

  def condition(firstVar: Variable, restVars: Variable*): Condition = Condition(firstVar +: restVars)

  def expression(firstVar: Variable, restVars: Variable*): Expression = new Expression(firstVar +: restVars)

  implicit class AugmentedVariableSeq(vars: Seq[Variable]) {
    def toFuncString(quoted: Boolean = false) = {
      val varNames = vars map { case named: NamedVar => named.name }
      val possiblyQuotedVarNames =
        if(quoted) varNames map (_.quoted)
        else varNames

      "(" + (possiblyQuotedVarNames mkString ", ") + ")"
    }
  }

  implicit class AugmentedString(str: String) {
    def quoted = "\"" + str + "\""
  }

}
