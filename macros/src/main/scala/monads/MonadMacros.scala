package monads

import scala.reflect.macros.blackbox.Context

class MonadMacros(val c: Context) {
  import c.universe._

  def bindImpl(f: c.Tree): c.Tree = {
    val obj = c.prefix
    q"""
      $obj >>= $f
    """
  }

  def fmapImpl(f: c.Tree): c.Tree = {
    val obj = c.prefix
    val x = freshParam("x")
    q"""
      $obj >>= { $x =>
        $obj._companion.`return`($f(${x.name}))
      }
    """
  }

  private def freshParam[T](name: String) =
    freshParamFromFunction(name, q"{ x => x }")

  private def freshParamFromFunction(name: String, function: Function) = {
    val Function(valDef :: Nil, _) = function
    val valName = TermName(c.freshName(name))
    ValDef(valDef.mods, valName, valDef.tpt, valDef.rhs)
  }
}
