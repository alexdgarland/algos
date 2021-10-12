package crackingthecodinginterview.stacksandqueues

case class DoubleStackQueue[T]
(
  private val newestFirstStack: Stack[T] = Stack[T](),
  private val oldestFirstStack: Stack[T] = Stack[T]()
) extends Queue[T] {

  override def length: Int = newestFirstStack.size + oldestFirstStack.size

  override def enqueue(value: T): Unit = newestFirstStack.push(value)

  private def ensureOldestValuesAvailable(): Unit = {
    if (oldestFirstStack.isEmpty) {
      while (newestFirstStack.peek().isDefined) {
        newestFirstStack.pop().foreach(oldestFirstStack.push)
      }
    }
  }

  override def peek(): Option[T] = {
    ensureOldestValuesAvailable()
    oldestFirstStack.peek()
  }

  override def dequeue(): Option[T] = {
    ensureOldestValuesAvailable()
    oldestFirstStack.pop()
  }

}
