package zipper


object ZipperTest {
  private sealed trait Tree[+A]
  private case object Empty extends Tree[Nothing]
  private case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]

  private implicit class TreeZipperOps[A](private val zipper: Zipper[Tree[A]]) extends AnyVal {
    def goLeft: Option[Zipper[Tree[A]]] =
      ifNode { node =>
        zipper.goDown(node.left, l => Node(node.value, l, node.right))
      }

    def goRight: Option[Zipper[Tree[A]]] =
      ifNode { node =>
        zipper.goDown(node.right, r => Node(node.value, node.left, r))
      }

    def updateValue(newValue: A): Option[Zipper[Tree[A]]] =
      ifNode { node =>
        zipper.updateTo(Node(newValue, node.left, node.right))
      }

    private def ifNode(f: Node[A] => Zipper[Tree[A]]): Option[Zipper[Tree[A]]] =
      zipper.focus match {
        case Empty => None
        case n@Node(_, _, _) => Some(f(n))
      }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Char] =
      Node('A',
        Node('B',
          Node('C',
            Empty,
            Empty,
          ),
          Node('D',
            Empty,
            Empty,
          )
        ),
        Node('E',
          Node('F',
            Empty,
            Empty,
          ),
          Node('G',
            Empty,
            Empty,
          )
        )
      )

    val newTree = Zipper(tree)
      .flatMap(_.goRight)
      .flatMap(_.goLeft)
      .flatMap(_.updateValue('Z'))
      .map(_.goTop.focus)

    assert(newTree.contains(
      Node('A',
        Node('B',
          Node('C',
            Empty,
            Empty,
          ),
          Node('D',
            Empty,
            Empty,
          )
        ),
        Node('E',
          Node('Z',
            Empty,
            Empty,
          ),
          Node('G',
            Empty,
            Empty,
          )
        )
      )
    ))

    println("OK!")
  }
}
