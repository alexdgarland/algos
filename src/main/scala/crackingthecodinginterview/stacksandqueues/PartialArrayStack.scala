package crackingthecodinginterview.stacksandqueues

import scala.reflect.ClassTag

case class PartialArrayStack[T]
(
  private val array: Array[T],
  private val minArrayIndex: Int,
  private val maxArrayIndex: Int
) extends Stack[T] {

  private var currentTopIndex: Option[Int] = None

  override def push(value: T): Unit = {
    val index = currentTopIndex.map(_ + 1).getOrElse(minArrayIndex)
    if (index > maxArrayIndex) throw new IllegalStateException("Cannot add new element to stack - max size exceeded")
    array(index) = value
    currentTopIndex = Some(index)
  }

  override def pop(): Option[T] = {
    val poppedValue = peek()
    currentTopIndex = currentTopIndex.flatMap(i => if (i == 0) None else Some(i-1))
    poppedValue
  }

  override def peek(): Option[T] = currentTopIndex.map(array(_))

  override def isEmpty: Boolean = currentTopIndex.isEmpty

  override def toList: List[T] = currentTopIndex
    .map(i => array.slice(minArrayIndex, i + 1).reverse.toList)
    .getOrElse(List())
}

case class TripleStack[T]
(
  stack1: PartialArrayStack[T],
  stack2: PartialArrayStack[T],
  stack3: PartialArrayStack[T]
)

object TripleStack {

  def apply[T: ClassTag](maxStackSize: Int = 10): TripleStack[T] = {
    val backingArray = new Array[T](maxStackSize * 3)
    TripleStack(
      PartialArrayStack(backingArray, 0, maxStackSize -1),
      PartialArrayStack(backingArray, maxStackSize, (maxStackSize * 2) -1),
      PartialArrayStack(backingArray, maxStackSize * 2, (maxStackSize * 3) -1)
    )
  }

}
