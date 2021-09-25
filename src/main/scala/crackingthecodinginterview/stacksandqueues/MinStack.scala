package crackingthecodinginterview.stacksandqueues

trait MinStack[T] extends Stack[T] {
  def min: Option[T]
}

class LinkedListMinStack[T]()(implicit ordering: Ordering[T]) extends LinkedListStack[T] with MinStack[T] {

  private val minValuesStack = Stack[T]()

  override def push(value:  T): Unit = {
    import ordering.mkOrderingOps
    if (!min.exists(value > _)) minValuesStack.push(value)
    super.push(value)
  }

  override def pop(): Option[T] = {
    if(peek() == minValuesStack.peek()) minValuesStack.pop()
    super.pop()
  }

  override def min: Option[T] = minValuesStack.peek()

}

object MinStack {

  def apply[T]()(implicit ordering: Ordering[T]): MinStack[T] = new LinkedListMinStack[T]()

}
