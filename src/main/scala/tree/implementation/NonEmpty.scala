package tree.implementation

import tree.traits.IntTree

case class NonEmpty(val elem: Int, left: IntTree, right: IntTree) extends BinaryTree :

  override def isEmpty = false

  override def root: Int = elem

  override infix def contains(i: Int): Boolean =
    if (i < elem) left.contains(i)
    else if (i > elem) right.contains(i)
    else true // i == elem

  override def insert(i: Int): IntTree =
    if (i < elem) NonEmpty(elem, left.insert(i), right)
    else if (i > elem) NonEmpty(elem, left, right.insert(i))
    else this

  override def size: Int = 1 + left.size + right.size

  override def height: Int = math.max(
    left match {
      case Empty => -1
      case nonEmpty => nonEmpty.height
    },
    right match {
      case Empty => -1
      case nonEmpty => nonEmpty.height
    }
  ) + 1
