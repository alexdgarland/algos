package crackingthecodinginterview.stacksandqueues

class FiniteStack[T](val capacity: Int)(implicit ordering: Ordering[T]) extends LinkedListStack[T] {

  private var currentSize = 0

  override def push(value: T): Unit = {
    if (isFull) throw new IllegalStateException(s"Array cannot store more than $capacity elements")
    currentSize += 1
    super.push(value)
  }

  override def pop(): Option[T] = {
    currentSize = if (currentSize <= 0) 0 else currentSize - 1
    super.pop()
  }

  def isFull: Boolean = currentSize >= capacity

}

class SetOfStacks[T](val innerStackCapacity: Int)(implicit ordering: Ordering[T]) extends Stack[T] {

  private val innerStacks = Stack[FiniteStack[T]]()

  private def currentStack: Option[FiniteStack[T]] = innerStacks.peek()

  private def forCurrentStack[TT](f: FiniteStack[T] => Option[TT]): Option[TT] = currentStack.flatMap(f)

  override def push(value: T): Unit = {
    if (currentStack.forall(_.isFull)) {
      innerStacks.push(new FiniteStack[T](innerStackCapacity))
    }
    currentStack.foreach(_.push(value))
  }

  override def pop(): Option[T] = forCurrentStack { stack =>
    val poppedValue = stack.pop()
    if (stack.isEmpty) {
      innerStacks.pop()
    }
    poppedValue
  }

  override def peek(): Option[T] = forCurrentStack(_.peek())

  override def isEmpty: Boolean = innerStacks.isEmpty

  override def toList: List[T] = innerStacks.toList.flatMap(_.toList)

}
