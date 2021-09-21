package crackingthecodinginterview.linkedlists

import scala.annotation.tailrec
import scala.collection.immutable

case class SinglyLinkedList[T](var head: Option[SinglyLinkedNode[T]] = None)(implicit ordering: Ordering[T])
  extends LinkedList[T, SinglyLinkedNode[T], SinglyLinkedList[_]] {

  /** *
   * Append a new value to the end of the linked-list.
   *
   * Runs in O(n) where n is the length of the existing list, as we have to touch each element on the way through.
   *
   * @param value The value to insert.
   */
  def append(value: T): Unit = {
    val newNode = Some(SinglyLinkedNode(value))
    head match {
      case None => head = newNode
      case Some(existingNode) =>
        var currentNode = existingNode
        while (currentNode.next.isDefined) {
          currentNode = currentNode.next.get
        }
        currentNode.next = newNode
    }
  }

  def prepend(value: T): Unit = head = Some(SinglyLinkedNode(value, head))

  def map[TT](f: T => TT)(implicit ordering: Ordering[TT]): SinglyLinkedList[TT] = {
    new ListMapper[T, SinglyLinkedNode[T], SinglyLinkedList[T], TT, SinglyLinkedNode[TT], SinglyLinkedList[TT]]() {
      override def newList(): SinglyLinkedList[TT] = SinglyLinkedList()

      override def newNode(value: TT, previousNode: Option[SinglyLinkedNode[TT]]): SinglyLinkedNode[TT] =
        SinglyLinkedNode(value)
    }.map(this, f)
  }

  override protected def deleteNextNode(beforeNode: SinglyLinkedNode[T]): Unit = {
    beforeNode.next = beforeNode.next.get.next
  }

  override protected def initialAssignForDeleteWhere(firstRetainedNodeOption: Option[SinglyLinkedNode[T]]): Unit = {
    head = firstRetainedNodeOption
  }

  override protected def insertAfter(value: T, beforeNode: SinglyLinkedNode[T]): Unit = {
    beforeNode.next = Some(SinglyLinkedNode(value, beforeNode.next))
  }

  override private[linkedlists] def setToTail(node: Option[SinglyLinkedNode[T]]): Unit = {}

  /**
   * Implement for singly-linked list using existing methods for length and retrieve-by-index (apply), both of which run in O(n).
   *
   * As far as I can see, this is equivalent in complexity to the "runner pointer" solution from CTCI but makes more sense.
   *
   * @param k reverse-index to retrieve node for
   * @return Option of node - if index is out of range None, otherwise Some(node)
   */
  override def kthFromLast(k: Int): Option[SinglyLinkedNode[T]] = this (this.length - (k + 1))

  override def partition(partitionValue: T): Unit = {
    import ordering.mkOrderingOps
    val leftBuilder = SinglyLinkedList.ListBuilder()
    val rightBuilder = SinglyLinkedList.ListBuilder()
    var currentNodeOption: Option[SinglyLinkedNode[T]] = head
    while (currentNodeOption.isDefined) {
      val builder = if (currentNodeOption.exists(_.value < partitionValue)) leftBuilder else rightBuilder
      builder.addNodeOption(currentNodeOption)
      currentNodeOption = currentNodeOption.flatMap(_.next)
    }
    head = (leftBuilder + rightBuilder).build().head
  }

  /**
   * Perform an in-place reversal of the nodes in the list.
   */
  override def reverse(): Unit = {
    var currentNode = head
    var previousNode: Option[SinglyLinkedNode[T]] = None
    while (currentNode.isDefined) {
      val nextNode = currentNode.get.next
      currentNode.get.next = previousNode
      previousNode = currentNode
      currentNode = nextNode
    }
    head = previousNode
  }

  /**
   * Indicate whether a linked list is a palindrome (values of nodes are the same reverse as forward.
   *
   * @return
   */
  override def isPalindrome: Boolean = {

    case class Accumulator(stack: List[T] = List(), nonMatchFound: Boolean = false)

    type AccumulatorFunc = (T, List[T]) => Accumulator

    case class IterState(node: Option[SinglyLinkedNode[T]], acc: Accumulator = Accumulator()) {
      def next(f: AccumulatorFunc): IterState = IterState(node.flatMap(_.next), f(node.get.value, acc.stack))
    }

    // Do a single pass through the list in O(n) to get the length - this is (~) equivalent to using a runner pointer
    val length = this.length

    // We then run this function twice, but each recursive invocation only runs over half the list
    // Given that each single invocation is constant-time (stack-style operations on Scala List only),
    // the overall time complexity here is also O(n).
    @tailrec
    def execForLength(state: IterState, f: AccumulatorFunc, remainingLength: Int = length / 2): IterState = {
      if (remainingLength == 0 || state.acc.nonMatchFound) state
      else execForLength(state.next(f), f, remainingLength - 1)
    }

    val stackingResult = execForLength(IterState(head), (value, stack) => Accumulator(value :: stack))

    val firstCheckNode = stackingResult.node.flatMap(if (length % 2 == 1) _.next else Some(_))

    val foundAnyNonMatch = execForLength(
      IterState(firstCheckNode, stackingResult.acc),
      (value, stack) => Accumulator(stack.tail, stack.head != value)
    ).acc.nonMatchFound

    !foundAnyNonMatch
  }

  /**
   * Works out whether two singly-linked lists intersect.
   *
   * Example (where the letters represent node references, NOT values:
   *
   * List 1 = A -> B -> C -> D -> E
   *
   * List 2 = C -> D -> E
   *
   * If they do, returns (a Some Option of) the intersecting node - in above case this would be node C.
   *
   * If not, return None.
   *
   * @param other The list to check for intersection with
   * @return
   */
  def intersectingNode(other: SinglyLinkedList[T]): Option[SinglyLinkedNode[T]] = {
    // Convert one list to a map in O(n) for that list
    val refMap = foldLeftNodes(immutable.HashMap[SinglyLinkedNode[T], Boolean]())((map, value) => map + (value -> true))
    // Walk through other list, also in O(n) for that list, checking map each iteration in constant time
    other.findNode(refMap.contains)
  }

  def getLoopStart: Option[SinglyLinkedNode[T]] = {
    @tailrec
    def inner
    (
      nodeOption: Option[SinglyLinkedNode[T]],
      seenMap: immutable.HashMap[SinglyLinkedNode[T], Boolean]
    ): Option[SinglyLinkedNode[T]] =
      nodeOption match {
        case None => None
        case Some(node) =>
          if (seenMap.contains(node))
            Some(node)
          else
            inner(node.next, seenMap + (node -> true))
      }

    inner(head, immutable.HashMap[SinglyLinkedNode[T], Boolean]())
  }

}

object SinglyLinkedList {

  /** *
   * Create a singly-linked list in O(n).
   *
   * @param values Values for the list
   * @tparam T Type of the values
   * @return
   */
  def apply[T](values: T*)(implicit ordering: Ordering[T]): SinglyLinkedList[T] = {
    val nodes = values.map(SinglyLinkedNode(_))
    (1 until nodes.length).foreach { i => nodes(i - 1).next = Some(nodes(i)) }
    SinglyLinkedList(nodes.headOption)
  }

  /**
   * Deletes a middle (non-head/ non-tail) node from a linked-list given a reference to the node only
   * (NOT given a reference to list/ head/ parent node)
   *
   * @param node The node to delete
   * @tparam T Type of values within the node
   */
  def deleteNode[T](node: SinglyLinkedNode[T]): Unit = {
    node.next match {
      case None =>
        throw new IllegalArgumentException("Cannot delete final node of list without access to full list")
      case Some(nextNode) =>
        node.value = nextNode.value
        node.next = nextNode.next
    }
  }

  protected case class ListBuilder[T]
  (
    var head: Option[SinglyLinkedNode[T]] = None,
    var latest: Option[SinglyLinkedNode[T]] = None
  )(implicit ordering: Ordering[T]) {

    def addNodeOption(nodeOption: Option[SinglyLinkedNode[T]]): Unit = {
      if (head.isEmpty) head = nodeOption
      latest.foreach(_.next = nodeOption)
      latest = nodeOption
    }

    def addValue(value: T): Unit = addNodeOption(Some(SinglyLinkedNode(value)))

    def +(that: ListBuilder[T]): ListBuilder[T] = {
      addNodeOption(that.head)
      latest = that.latest
      this
    }

    def build(): SinglyLinkedList[T] = {
      latest.foreach(_.next = None)
      SinglyLinkedList(head)
    }

  }

  /**
   * Take two lists of ints, representing a number with each node as a single digit in reverse order,
   * and add them together maintaining the same representation in an efficient way (O(n)).
   *
   * @param list1 First list to add
   * @param list2 Second list to add
   * @return
   */
  def sumLists(list1: SinglyLinkedList[Int], list2: SinglyLinkedList[Int]): SinglyLinkedList[Int] = {
    new IntListSummation[SinglyLinkedNode[Int], SinglyLinkedList[Int], SinglyLinkedList.ListBuilder[Int]] {
      override def newBuilder(): ListBuilder[Int] = SinglyLinkedList.ListBuilder[Int]()

      override def addValue(builder: ListBuilder[Int], value: Int): Unit = builder.addValue(value)

      override def build(builder: ListBuilder[Int]): SinglyLinkedList[Int] = builder.build()
    }.sumLists(list1, list2)
  }

}
