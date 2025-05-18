package tree.implementation

import tree.traits.IntTree

import scala.annotation.tailrec

/**
  * A companion object for the singly linked list.
  * This enables creating lists list this: val list = SinglyLinkedIntList(1,2,3)
  * which results in Cons(1,Cons(2,Cons(3,Empty))))
  */
object BinaryTree :

  /** The apply function is a special function in scala.
    * It can be invoked with SinglyLinkedIntList.apply(args) or simply SinglyLinkedIntList(args).
    * This particular implementation of it is also a variadic function, i.e.
    * a function which accepts one or more arguments of the same type (integers) as parameters.
    */
  //inside this method xs is of type Seq[int]
  def apply(xs: Int*): BinaryTree =

    @tailrec
    def treeIntialization(tree:BinaryTree, xs: Int*):BinaryTree= xs match

    case Seq() => tree
    //: _* results in the sequence being passed as multiple parameters - (1,2,3) instead of Seq[Int]{1,2,3}
    case _ => treeIntialization(tree.insert(xs.head).asInstanceOf[BinaryTree],xs.tail*)

    treeIntialization(Empty,xs*)

abstract class BinaryTree extends IntTree :

  /* Helper function used by the delete-operation
  *  Function finds the node with the next higher value
  *  compared to the root node of the binary tree (see tests)
  * */
  def findSuccessor: BinaryTree = this match {
    case Empty => throw new Error("Cannot find successor in empty tree")
    case NonEmpty(_, _, right) =>
      @tailrec
      def findMin(tree: IntTree): BinaryTree = tree match {
        case Empty => throw new Error("No minimum in empty tree")
        case NonEmpty(_, left, _) if left.isEmpty => tree.asInstanceOf[BinaryTree]
        case NonEmpty(_, left, _) => findMin(left)
      }

      if (right.isEmpty) throw new Error("No successor available")
      else findMin(right)
  }

  override def delete(i: Int): IntTree = this match {
    case Empty => throw new Error(s"Element $i not found in tree")
    case NonEmpty(elem, left, right) =>
      if (i < elem) NonEmpty(elem, left.delete(i), right)
      else if (i > elem) NonEmpty(elem, left, right.delete(i))
      else {
        if (left.isEmpty) right
        else if (right.isEmpty) left
        else {
          try {
            val successor = right.asInstanceOf[BinaryTree].findSuccessor
            NonEmpty(successor.root, left, right.delete(successor.root))
          } catch {
            case e: Error => throw new Error(s"Cannot delete node $i: $e")
          }
        }
      }
  }

  override def map(mapFun: Int => Int): IntTree = this match {
    case Empty => Empty
    case NonEmpty(elem, left, right) =>
      NonEmpty(mapFun(elem), left.map(mapFun), right.map(mapFun))
  }

  override def filter(filterFun: Int => Boolean): IntTree = this match {
    case Empty => Empty
    case NonEmpty(elem, left, right) =>
      if (filterFun(elem)) {
        NonEmpty(elem, left.filter(filterFun), right.filter(filterFun))
      } else {
        merge(left.filter(filterFun), right.filter(filterFun))
      }
  }

  private def merge(t1: IntTree, t2: IntTree): IntTree = (t1, t2) match {
    case (Empty, t) => t
    case (t, Empty) => t
    case (_, _) =>
      t2.tree2List.foldLeft(t1)((tree, elem) => tree.insert(elem))
  }

  override def tree2List: List[Int] = this match {
    case Empty => List()
    case NonEmpty(elem, left, right) =>
      left.tree2List ++ List(elem) ++ right.tree2List
  }

  override def isBinaryTree: Boolean = this match {
    case Empty => true
    case NonEmpty(elem, left, right) =>
      val leftValid = left.isEmpty ||
        (left.asInstanceOf[NonEmpty].elem < elem &&
          left.isBinaryTree &&
          left.tree2List.forall(_ < elem))

      val rightValid = right.isEmpty ||
        (right.asInstanceOf[NonEmpty].elem > elem &&
          right.isBinaryTree &&
          right.tree2List.forall(_ > elem))

      leftValid && rightValid
  }
