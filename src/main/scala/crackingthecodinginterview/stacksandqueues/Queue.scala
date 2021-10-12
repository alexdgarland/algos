package crackingthecodinginterview.stacksandqueues

trait Queue[T] {

  def length: Int

  def enqueue(value: T): Unit

  def peek(): Option[T]

  def dequeue(): Option[T]

}
