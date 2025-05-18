package list.implementation

import list.traits.IntList

/**
 * Represents a non-empty singly linked list with a head element and a tail list.
 *
 * @param head the first element of the list
 * @param tail the rest of the list
 */
case class Cons (head: Int, tail: IntList) extends SinglyLinkedIntList :

  override def isEmpty = false

  override def get(index: Int): Int = index match
    case 0 => head
    case i => tail.get(i - 1)

  override def append(elem: Int): IntList = Cons(head, tail.append(elem))

  override def contains(elem: Int): Boolean = if (elem==head) true else tail.contains(elem)

  override def prepend(elem: Int): IntList = Cons(elem,this)

  override def delete(elem: Int): IntList = {
    if (head == elem) tail
    else Cons(head, tail.delete(elem))
  }

  override def deleteAll(elem: Int): IntList = this match {
    case Empty => Empty
    case Cons(head, tail) => {
      if (head == elem) tail.deleteAll(elem)
      else Cons(head, tail.deleteAll(elem))
    }
  }


