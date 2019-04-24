package treeset

import treeset.PrintGrid.Element

object PrintGrid {
  def main(args: Array[String]): Unit =
    for {
      sz <- 0 until 32
      it = Iterator.from(1).take(sz)
      set = TreeSet.fromSortedDistinctIterator(it)
      grid <- PrintGrid(set)
    } {
      grid.print()
      println()
    }

  private def apply(set: TreeSet[Int]): Option[PrintGrid] =
    set match {
      case TreeSet.Empty() =>
        None
      case TreeSet.Node(left, value, right) =>
        val root = Element(value.toString)
        (PrintGrid(left), PrintGrid(right)) match {
          case (Some(leftGrid), Some(rightGrid)) =>
            val shiftedRightGrid = rightGrid.shift(leftGrid.width + root.width)
            val Array(leftRoot) = leftGrid.rows(0)
            val Array(rightRoot) = shiftedRightGrid.rows(0)
            def between(a: Element, b: Element)(x: Element): Element = x.leftAt((a.right + b.left - x.width) / 2)
            val shiftedRoot = between(leftRoot, rightRoot)(root)
            def connector(node: Element, subNode: Element): Element =
              if (subNode.right <= node.left) {
                node.left - subNode.right match {
                  case 0 | 1 => Element("/").rightAt(node.left)
                  case 2 => Element("/").leftAt(subNode.right)
                  case gap => Element("/" ++ ("-" * (gap - 2)) ++ "/", left = subNode.right)
                }
              } else if (node.right <= subNode.left) {
                subNode.left - node.right match {
                  case 0 | 1 => Element("\\").leftAt(node.right)
                  case 2 => Element("\\").rightAt(subNode.left)
                  case gap => Element("\\" ++ ("-" * (gap - 2)) ++ "\\", left = node.right)
                }
              } else {
                fail()
              }
            val leftConnector = connector(shiftedRoot, leftRoot)
            val rightConnector = connector(shiftedRoot, rightRoot)
            val rows = leftGrid.rows.zipAll(shiftedRightGrid.rows, Array.empty[Element], Array.empty[Element]).map { case (l, r) => l ++ r }
            Some(
              PrintGrid(
                rows = Array(
                  Array(shiftedRoot),
                  Array(leftConnector, rightConnector),
                ) ++ rows
              )
            )
          case (Some(leftGrid), None) =>
            val Array(leftRoot) = leftGrid.rows(0)
            val shiftedRoot = root.leftAt(leftRoot.right)
            val connector = Element("/").rightAt(shiftedRoot.left)
            Some(
              PrintGrid(
                rows = Array(
                  Array(shiftedRoot),
                  Array(connector),
                ) ++ leftGrid.rows
              )
            )
          case (None, None) =>
            Some(PrintGrid(Array(Array(root))))
          case _ =>
            fail()
        }
    }

  case class Element(content: String, left: Int = 0) {
    val width: Int = content.length
    val right: Int = left + width
    def shift(n: Int): Element = copy(left = left + n)
    def leftAt(l: Int): Element = copy(left = l)
    def rightAt(r: Int): Element = copy(left = r - content.length)
  }

  private def fail(): Nothing =
    assert(false).asInstanceOf[Nothing]
}

case class PrintGrid(rows: Array[Array[Element]]) {
  val left: Int = rows.map(_.map(_.left).min).min
  val right: Int = rows.map(_.map(_.right).max).max
  val width: Int = right - left

  def shift(n: Int): PrintGrid =
    PrintGrid(
      rows.map { row =>
        row.map { elem =>
          elem.shift(n)
        }
      }
    )

  def print(): Unit =
    for (row <- rows) {
      val line = row.foldLeft("") { (line, elem) =>
        line + (" " * (elem.left - line.length)) + elem.content
      }
      println(line)
    }
}
