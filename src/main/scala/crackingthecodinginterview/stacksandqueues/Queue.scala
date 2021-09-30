package crackingthecodinginterview.stacksandqueues

trait Queue[T] {

  def size: Int

  def add(value: T): Unit

  def peek(): Option[T]

  def remove(): Option[T]

}

case class DoubleStackQueue[T]
(
  private val newestFirstStack: Stack[T] = Stack[T](),
  private val oldestFirstStack: Stack[T] = Stack[T]()
) extends Queue[T] {

  // TODO - Stack (trait + impls) doesn't currently have a direct `size` method - maybe need to impl this
  override def size: Int = newestFirstStack.toList.size + oldestFirstStack.toList.size

  override def add(value: T): Unit = newestFirstStack.push(value)

  private def ensureOldestValuesAvailable(): Unit = {
    if(oldestFirstStack.isEmpty) {
      while (newestFirstStack.peek().isDefined) {
        newestFirstStack.pop().foreach(oldestFirstStack.push)
      }
    }
  }

  override def peek(): Option[T] = {
    ensureOldestValuesAvailable()
    oldestFirstStack.peek()
  }

  override def remove(): Option[T] = {
    ensureOldestValuesAvailable()
    oldestFirstStack.pop()
  }

}

object Queue {

  def apply[T](): Queue[T] = DoubleStackQueue[T]()

}
