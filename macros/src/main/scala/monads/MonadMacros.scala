package monads

import scala.reflect.macros.blackbox.Context

class MonadMacros(val c: Context) {
  import c.universe._

  def fmapImpl(f: c.Tree): c.Tree = {
    val obj = c.prefix
    val x = freshParam("x")
    q"""
      $obj >>= { $x =>
        $obj._companion.`return`($f(${x.name}))
      }
    """
  }

  def joinImpl()(ev: c.Tree): c.Tree = {
    val obj = c.prefix
    val x = freshParam("x")
    q"""
      $obj >>= { $x => ${x.name} }
    """
  }

  def composeImpl(g: c.Tree): c.Tree = {
    val obj = c.prefix
    val a = freshParam("a")
    val b = freshParam("b")
    q"""
      { $a =>
        $obj.f(${a.name}) >>= { $b => $g(${b.name}) }
      }
    """
  }

  def liftM2Impl[A, B]
      (ma: c.Tree, mb: c.Tree)
      (implicit ttA: c.WeakTypeTag[A], ttB: c.WeakTypeTag[B]): c.Tree = {
    val obj = c.prefix
    val a = freshParam("a", ttA)
    val b = freshParam("b", ttB)
    q"""
      $ma >>= { $a =>
        $mb >>= { $b =>
          $ma._companion.`return`($obj.f(${a.name}, ${b.name}))
        }
      }
    """
  }

  private def freshParam[T](name: String) =
    freshParamFromFunction(name, q"{ x => x }")

  private def freshParam[T](name: String, tpt: c.WeakTypeTag[T]) =
    freshParamFromFunction(name, q"{ x: $tpt => x }")

  private def freshParamFromFunction(name: String, function: Function) = {
    val Function(valDef :: Nil, _) = function
    val valName = TermName(c.freshName(name))
    ValDef(valDef.mods, valName, valDef.tpt, valDef.rhs)
  }
}