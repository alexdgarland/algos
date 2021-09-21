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
    // TODO - Should fail (how do we handle this - Exception or Either?) if overflows past maxArrayIndex
    val index = currentTopIndex.map(_ + 1).getOrElse(minArrayIndex)
    array(index) = value
    currentTopIndex = Some(index)
  }

  // None if empty (how do we represent this? including as starting state...), otherwise Some(value at index)
  // Decrement currentTopIndex, setting to None once we empty the stack
  override def pop(): Option[T] = ???

  override def peek(): Option[T] = ???

  override def isEmpty: Boolean = ???

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
