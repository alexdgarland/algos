package crackingthecodinginterview.stacksandqueues

import crackingthecodinginterview.linkedlists.DoublyLinkedList

/**
 * Queue implementation using a DoublyLinkedList.
 *
 * The instructions for this question suggest using the built-in linked list implementation for given language.
 * This would naturally be the Java LinkedList (as Scala deprecates direct usage of linked lists as being too low-level).
 * However, the Java version implements Deque (and hence Queue) interfaces, and so would make queue operations trivial.
 * Hence am building on top of my own DoublyLinkedList implementation.
 *
 * @tparam T Type for values
 */
class LinkedListQueue[T]() extends DoublyLinkedList[T]() with Queue[T] {

  override def enqueue(value: T): Unit = prepend(value)

  override def dequeue(): Option[T] = {
    val peekedValue = peek()
    tail.foreach { tailNode =>
      tailNode.prev match {
        case None =>
          head = None
        case Some(newTailNode) =>
          newTailNode.next = None
      }
      tail = tailNode.prev
    }
    peekedValue
  }

  override def peek(): Option[T] = tail.map(_.value)

}