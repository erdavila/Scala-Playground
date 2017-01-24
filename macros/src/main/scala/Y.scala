import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

object Y {
  def m(g: () => Int): Unit = macro impl

  def impl(c: Context)(g: c.Tree): c.Tree = {
    import c.universe._
    q"{ () => $g() }.apply()"
  }
}
