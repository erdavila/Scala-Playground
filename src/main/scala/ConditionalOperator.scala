import scala.collection.mutable
import scala.language.implicitConversions

object ConditionalOperator {
  object General {
    class Condition[+T](cond: Boolean, trueBlock: => T) {
      private val trueValue: Option[T] = if(cond) Some(trueBlock) else None
      def apply[R >: T](falseBlock: => R): R = trueValue getOrElse falseBlock
      def toOption: Option[T] = trueValue
    }

    implicit class ConditionWrapper(cond: Boolean) {
      def ?? [T](trueBlock: => T): Condition[T] = new Condition(cond, trueBlock)
    }

    implicit def condition2Option[T](cond: Condition[T]): Option[T] = cond.toOption
  }

  object Specialized {
    trait Condition[+T] {
      def apply[R >: T](falseBlock: => R): R
      def toOption: Option[T]
    }

    class TrueCondition[T](trueValue: T) extends Condition[T] {
      def apply[R >: T](falseBlock: => R) = trueValue
      def toOption: Option[T] = Some(trueValue)
    }

    class FalseCondition[T] extends Condition[T] {
      def apply[R >: T](falseBlock: => R) = falseBlock
      def toOption: Option[T] = None
    }

    implicit class ConditionWrapper(cond: Boolean) {
      def ?? [T](trueBlock: => T): Condition[T] = if (cond) new TrueCondition(trueBlock) else new FalseCondition()
    }

    implicit def condition2Option[T](cond: Condition[T]): Option[T] = cond.toOption
  }

  //val Implementation = General
  val Implementation = Specialized
  import Implementation._

  implicit class FalseBlockWrapper[F](falseBlock: => F) {
    def :: [R >: F](condition: Condition[R]): R = condition(falseBlock)
  }

  class Base
  object Base {
    val instance = new Base
    def apply() = instance
  }

  case class Derived1() extends Base
  case class Derived2() extends Base

  type Parent = Base
  val  Parent = Base
  type Child = Derived1
  val  Child = Derived1
}

object ConditionalOperatorTest extends App {
  import ConditionalOperator._
  import ConditionalOperator.Implementation._

  // ==
  val x: Int = true ?? 1 :: 2
  assert(x == 1)

  // ==
  val y: Char = false ?? 'A' :: 'B'
  assert(y == 'B')

  // <>
  val v1: Base = true ?? Derived1() :: Derived2()
  assert(v1 == Derived1())

  // <>
  val v2: Base = false ?? Derived1() :: Derived2()
  assert(v2 == Derived2())

  // >
  val pt: Parent = true ?? Parent() :: Child()
  assert(pt == Parent())

  // >
  val cf: Parent = false ?? Parent() :: Child()
  assert(cf == Child())

  // <
  val ct: Parent = true ?? Child() :: Parent()
  assert(ct == Child())

  // <
  val pf: Parent = false ?? Child() :: Parent()
  assert(pf == Parent())

  val optT: Option[Int] = true ?? 7
  assert(optT == Some(7))

  val optF: Option[Int] = false ?? 7
  assert(optF == None)

  {
    val executedBlocks = mutable.Set.empty[Boolean]
    true ?? { executedBlocks += true } :: { executedBlocks += false }
    assert(executedBlocks == Set(true))
  }

  {
    val executedBlocks = mutable.Set.empty[Boolean]
    false ?? { executedBlocks += true } :: { executedBlocks += false }
    assert(executedBlocks == Set(false))
  }

  // ==
  {
    // true ?? 1 :: 2
    type T = Int
    type F = Int
    type R = Int
    val t: T = 1
    val f: F = 2
    val c: Condition[T] = ConditionWrapper(true).??[T](t)    // T = Int
    val fbw: FalseBlockWrapper[F] = FalseBlockWrapper[F](f)  // F = Int
    val r: R = fbw.::[R](c)                                  // R = Int >: Int = F
    val rr: R = c.apply[R](f)                                // R = Int >: Int = T
  }

  // <>
  {
    // true ?? Derived1() :: Derived2()
    type T = Derived1
    type F = Derived2
    type R = Base
    val t: T = Derived1()
    val f: F = Derived2()
    val c: Condition[T] = ConditionWrapper(true).??[T](t)    // T = Derived1
    val fbw: FalseBlockWrapper[F] = FalseBlockWrapper[F](f)  // F = Derived2
    val r: R = fbw.::[R](c)                                  // R = Base >: Derived2 = F
    val rr: R = c.apply[R](f)                                // R = Base >: Derived1 = T
  }

  // >
  {
    // true ?? Parent() :: Child()
    type T = Parent
    type F = Child
    type R = Parent
    val t: T = Parent()
    val f: F = Child()
    val c: Condition[T] = ConditionWrapper(true).??[T](t)    // T = Parent
    val fbw: FalseBlockWrapper[F] = FalseBlockWrapper[F](f)  // F = Child
    val r: R = fbw.::[R](c)                                  // R = Parent >: Child = F
    val rr: R = c.apply[R](f)                                // R = Parent >: Parent = T
  }

  // <
  {
    // true ?? Child() :: Parent()
    type T = Child
    type F = Parent
    type R = Parent
    val t: T = Child()
    val f: F = Parent()
    val c: Condition[T] = ConditionWrapper(true).??[T](t)    // T = Child
    val fbw: FalseBlockWrapper[F] = FalseBlockWrapper[F](f)  // F = Parent
    val r: R = fbw.::[R](c)                                  // R = Parent >: Parent = F
    val rr: R = c.apply[R](f)                                // R = Parent >: Child = T
  }

  println("OK!")
}
