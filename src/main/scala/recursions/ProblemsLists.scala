package recursions

import list.implementation.*
import list.traits.IntList

import scala.annotation.tailrec

object ProblemsLists:

  /*
   * Use only recursions for your solutions
   */

  /**
   *
   * Given a number i that should be duplicated a number of times
   * returns an IntList that contains the duplicated i
   *
   * E.x. duplicateNum(4,3)
   * -> SinglyLinkedList(4, 4, 4, 4)
   *
   * @param i     number to duplicate
   * @param times number of duplicates
   * @return List of duplicated numbers
   */
  def duplicateNum(i: Int, times: Int): IntList = {
    @tailrec
    def duplicateRecursive(remainingTimes: Int, current: IntList): IntList = remainingTimes match
      case n if n <= 0 => current
      case n => duplicateRecursive(n - 1, Cons(i, current))

    duplicateRecursive(times, Empty)
  }

  /**
   *
   * Given is an IntList l that contains arbitrary numbers and a predicate Function Int=>Boolean
   * All numbers of the list that fulfill the predicate should be duplicated
   * returns an IntList that contains all duplicated numbers that fulfill the predicate and the
   * remaining other numbers in the same order as they occur in the origin list
   *
   * E.x. duplicateEqualNumbers(x=>(x % 2==0),SinglyLinkedIntList(1,4,3,5,8))
   * -> SinglyLinkedIntList(1, 4, 4, 3, 5, 8, 8)
   *
   * Use only recursions to solve the problem and none of the higher order Functions
   *
   * @param predicate predicate functions
   * @param l         IntList that should be processed
   * @return IntList that contains the duplicates and all other nums
   */
  def duplicateNumbersFulfillingPredicate(predicate: Int => Boolean, l: IntList): IntList = l match
    case Empty => Empty
    case Cons(head, tail) =>
      val processedTail = duplicateNumbersFulfillingPredicate(predicate, tail)
      if (predicate(head)) Cons(head, Cons(head, processedTail))
      else Cons(head, processedTail)

  /**
   *
   * Given is an IntList l that contains arbitrary numbers
   * The function builds all subsets of the list l
   * returns a List of IntList that contains all subsets
   *
   * E.g. combinations(SinglyLinkedList(1,2,3))
   * -> Set(Cons(1, Cons(2, Cons(3, Empty))), Cons(1, Cons(2, Empty)), Cons(1, Cons(3, Empty)), Cons(1, Empty),
   * Cons(2, Cons(3, Empty)), Cons(2, Empty), Cons(3, Empty), Empty)
   *
   * @param l given list
   * @return List of all subsets
   */
  def combinations(l: IntList): List[IntList] = {
    def toScalaList(intList: IntList): List[Int] = {
      if (intList.isEmpty) Nil
      else intList.head :: toScalaList(intList.tail)
    }

    def toIntList(list: List[Int]): IntList = {
      list.foldRight(Empty: IntList)((elem, acc) => Cons(elem, acc))
    }

    val scalaList = toScalaList(l)

    def generateSubsets(remaining: List[Int]): List[List[Int]] = {
      remaining match {
        case Nil => List(Nil)
        case head :: tail =>
          val subsetsWithoutHead = generateSubsets(tail)
          val subsetsWithHead = subsetsWithoutHead.map(subset => head :: subset)
          subsetsWithoutHead ::: subsetsWithHead
      }
    }

    generateSubsets(scalaList).map(toIntList)
  }


