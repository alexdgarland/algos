package crackingthecodinginterview.stacksandqueues

import crackingthecodinginterview.linkedlists.SinglyLinkedList

trait Stack[T] {

  def push(value: T): Unit

  def pop(): Option[T]

  def peek(): Option[T]

  def isEmpty: Boolean

  def toList: List[T]

  def size: Int

}

object Stack {
  def apply[T]: Stack[T] = new LinkedListStack[T]()

  private def apply[T](list: SinglyLinkedList[T]): Stack[T] = {
    val stack = new LinkedListStack[T]()
    stack.head = list.head
    stack
  }

  def apply[T](values: T*): Stack[T] = Stack(SinglyLinkedList(values: _*))

  /**
   * Sort a stack using only one additional stack for temp storage.
   *
   * This is definitely not the most efficient way to do a sort - is effectively a bubble-sort running in O(n-squared) -
   * however not sure we can do a lot better while meeting the constraint to not use any other data structures.
   *
   * @param stack    Stack to be sorted.
   * @param ordering Ordering over the types of the stack values.
   * @tparam T Type of the stack values.
   */
  def sort[T](stack: Stack[T])(implicit ordering: Ordering[T]): Unit = {
    import ordering.mkOrderingOps
    val buffer = Stack[T]()
    while (!stack.isEmpty) {
      val current = stack.pop().get
      while (buffer.peek().exists(_ > current)) {
        buffer.pop().foreach(stack.push)
      }
      buffer.push(current)
    }
    while (!buffer.isEmpty) {
      buffer.pop().foreach(stack.push)
    }
  }

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

  override def size: Int = foldLeft(0)((acc, _) => acc + 1)

}
