package forcomprehensions

trait Variable

trait NamedVar extends Variable {
  val name: String
}

case class SimpleVar(name: String) extends NamedVar

case class AuxVar(name: String, usedVars: Seq[Variable]) extends NamedVar

object Pattern {
  def apply(variable: Variable): Pattern = variable match {
    case sv: SimpleVar => new Pattern(sv, Seq.empty)
  }

  def unapply(p: Pattern): Option[Variable] = Some(p.variable)
}

class Pattern(val variable: NamedVar, val usedVars: Seq[Variable]) extends Variable {
  override def equals(obj: Any): Boolean = obj match {
    case p: Pattern => this.variable == p.variable
    case _ => false
  }

  def toFuncString() = toString()

  override def toString() = "Pattern(" + variable.name + ")"
}

object IsTuple {
  def unapply(p: Product): Boolean = p.getClass().getName().startsWith("scala.Tuple")
}

case class Condition(vars: Seq[Variable]) {
  def toFuncString() = "condition" + vars.toFuncString()
}

class Expression(val vars: Seq[Variable]) {
  def toFuncString() = "expression" + vars.toFuncString()
  override def toString() = "Expression(" + vars + ")"
  override def equals(other: Any) = other match {
    case that: Expression => (this.vars == that.vars)
    case _ => false
  }
}

trait Monad[A] {
  val elem: A

  def map[B](func: A => B)(implicit handler: MapHandler[A, B]): Monad[B] = handler(this, func)

  def flatMap(func: A => Monad[Expression]): Monad[Expression] = {
    val resultMonad = func(elem)
    MonadFromCall(this, "flatMap", resultMonad.elem, FlatMapOperation(resultMonad))
  }

  def withFilter[B](func: A => B)(implicit handler: FilterHandler[A, B]): Monad[A] = handler(this, func)

  def toFuncString(): String
}

case class MonadFor(elem: Variable, usedVars: Seq[Variable]) extends Monad[Variable] {
  def toFuncString() = {
    val varName = elem match {
      case nv: NamedVar => nv.name
      case p: Pattern => p.variable.name
    }
    "monadFor(" + varName.quoted + ")" + usedVars.toFuncString()
  }
}

trait Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B): String

  protected def paramStr[A](elemFrom: A): String = elemFrom match {
    case nv: NamedVar => exprStr(nv)
    case elem => "case " + exprStr(elem)
  }

  protected def exprStr[A](elemFrom: A): String = elemFrom match {
    case nv: NamedVar => nv.name
    case p: Pattern => p.toFuncString()
    case tuple @ IsTuple() => "(" + (tuple.productIterator map exprStr mkString ", ") + ")"
  }
}

case object SimpleOperation extends Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B) = {
    val param = paramStr(elemFrom)
    val result = elemTo match {
      case expr: Expression => expr.toFuncString()
    }
    param + " => " + result
  }
}

case object PatternFilterOperation extends Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B): String =
    paramStr(elemTo) + " => true case _ => false"
}

case class FlatMapOperation(resultMonad: Monad[Expression]) extends Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B): String =
    paramStr(elemFrom) + " => " + resultMonad.toFuncString()
}

case class ConditionOperation(condition: Condition) extends Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B): String =
    paramStr(elemFrom) + " => " + condition.toFuncString()
}

case object AuxVarOperation extends Operation {
  def bodyString[A, B](elemFrom: A, elemTo: B): String = {
    val fromVarParam = paramStr(elemFrom)
    val fromVarExpr = exprStr(elemFrom)
    val newVars = elemTo match {
      case tuple @ IsTuple() =>
        assert(tuple.productElement(0) == elemFrom)
        tuple.productIterator.drop(1).map { case av: AuxVar => av }.toSeq
    }

    fromVarParam + " => " +
      (newVars map newVarStr mkString " ") +
      " (" + fromVarExpr + ", " + (newVars map (_.name) mkString ", ") + ")"
  }

  private def newVarStr(newVar: AuxVar): String =
    "val " + newVar.name + " = valueFor(" + newVar.name.quoted + ")" + newVar.usedVars.toFuncString() + ";"
}

case class MonadFromCall[Z, A](originMonad: Monad[Z], method: String, elem: A, op: Operation) extends Monad[A] {
  def toFuncString() =
    originMonad.toFuncString() + " " + method + " { " + op.bodyString(originMonad.elem, elem) + " }"
}

trait MapHandler[A, B] {
  def apply(monad: Monad[A], func: (A) => B): Monad[B]
}

object MapHandler {
  implicit def forAny_Expression[A] = new MapHandler[A, Expression] {
    def apply(monad: Monad[A], func: (A) => Expression): Monad[Expression] = {
      val expression = func(monad.elem)
      MonadFromCall(monad, "map", expression, SimpleOperation)
    }
  }

  implicit def forAny_Tuple[A, Tuple <: Product] = new MapHandler[A, Tuple] {
    def apply(monad: Monad[A], func: (A) => Tuple): Monad[Tuple] = {
      val vars = func(monad.elem)
      assert(IsTuple.unapply(vars))
      assert(vars.productElement(0) == monad.elem, vars.productElement(0) + "!=" + monad.elem)
      MonadFromCall(monad, "map", vars, AuxVarOperation)
    }
  }
}

trait FilterHandler[A, B] {
  def apply(monad: Monad[A], func: (A) => B): Monad[A]
}

object FilterHandler {
  implicit val forVariable_Boolean = new FilterHandler[Variable, Boolean] {
    def apply(monad: Monad[Variable], func: Variable => Boolean): Monad[Variable] = {
      val pattern = Pattern(monad.elem)
      MonadFromCall(monad, "withFilter", pattern, PatternFilterOperation)
    }
  }

  implicit def forAny_Condition[A] = new FilterHandler[A, Condition] {
    def apply(monad: Monad[A], func: (A) => Condition): Monad[A] = {
      val condition = func(monad.elem)
      MonadFromCall(monad, "withFilter", monad.elem, ConditionOperation(condition))
    }
  }
}
