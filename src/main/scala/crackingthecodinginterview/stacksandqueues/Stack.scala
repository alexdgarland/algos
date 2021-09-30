package crackingthecodinginterview.stacksandqueues

import crackingthecodinginterview.linkedlists.SinglyLinkedList

trait Stack[T] {

  def push(value: T): Unit

  def pop(): Option[T]

  def peek(): Option[T]

  def isEmpty: Boolean

  def toList: List[T]
}

object Stack {
  def apply[T]: Stack[T] = new LinkedListStack[T]()

  private def apply[T](list: SinglyLinkedList[T]): Stack[T] = {
    val stack = new LinkedListStack[T]()
    stack.head = list.head
    stack
  }

  def apply[T](values: T*): Stack[T] = Stack(SinglyLinkedList(values: _*))
}

class LinkedListStack[T] extends SinglyLinkedList[T] with Stack[T] {
  override def push(value: T): Unit = prepend(value)

  override def pop(): Option[T] = {
    val poppedValue = peek()
    head.foreach(currentHead => head = currentHead.next)
    poppedValue
  }

  override def peek(): Option[T] = head.map(_.value)

  override def isEmpty: Boolean = head.isEmpty

  override def toList: List[T] = toList()
}
