package crackingthecodinginterview.linkedlists

/***
 *
 * @param head First item in the list (may be null if list is empty).
 * @param tail Last item in the list (may be null if list is empty).
 * @tparam T Types of the values.
 */
case class DoublyLinkedList[T]
(
  var head: Option[DoublyLinkedNode[T]] = None,
  var tail: Option[DoublyLinkedNode[T]] = None
) extends LinkedList[T, DoublyLinkedNode[T], DoublyLinkedList[_]] {

  def toListReversed: List[T] = toList(tail, node => node.prev)

  def deleteWhere(predicate: T => Boolean): Unit = {
    new PredicateBasedNodeDeleter[T, DoublyLinkedNode[T], DoublyLinkedList[T]](this) {
      override protected def initialAssign(firstRetainedNodeOption: Option[DoublyLinkedNode[T]]): Unit = {
        firstRetainedNodeOption.foreach(_.prev = None)
        list.head = firstRetainedNodeOption
        list.tail = firstRetainedNodeOption
      }
      override protected def skipNode(nodeBeforeSkippable: DoublyLinkedNode[T]): Unit = {
        val skipToNode = nodeBeforeSkippable.next.get.next
        nodeBeforeSkippable.next = skipToNode
        skipToNode.foreach(_.prev = Some(nodeBeforeSkippable))
      }
      override protected def assignTail(potentialTailNode: DoublyLinkedNode[T]): Unit = {
        potentialTailNode.next.foreach(node => list.tail = Some(node))
      }
    }.deleteWhere(predicate)
  }

  override def prepend(value: T): Unit = {
    val newNode = Some(DoublyLinkedNode(value, head))
    head.foreach(_.prev = newNode)
    head = newNode
    if (tail.isEmpty) { tail = newNode }
  }

  /***
   * Insert to end of list in constant time.
   *
   * @param value Value to append to list.
   */
  override def append(value: T): Unit = {
    val newNode = Some(DoublyLinkedNode(value, None, tail))
    head match {
      case None => head = newNode
      case Some(_) => tail.foreach(_.next = newNode)
    }
    tail = newNode
  }

  override def map[TT](f: T => TT): DoublyLinkedList[TT] = {
    new ListMapper[T, DoublyLinkedNode[T], DoublyLinkedList[T], TT, DoublyLinkedNode[TT], DoublyLinkedList[TT]](this) {
      override def newList(): DoublyLinkedList[TT] = DoublyLinkedList()
      override def newNode(value: TT, previousNode: Option[DoublyLinkedNode[TT]]): DoublyLinkedNode[TT] =
        DoublyLinkedNode(value, None, previousNode)
      override def assignTail(node: DoublyLinkedNode[TT]): Unit = mappedList.tail = Some(node)
    }.map(f)
  }

  override def insertAt(value: T, index: Int): Unit = {
    if (index < 0) { throw new IndexOutOfBoundsException("Cannot insert to a negative index") }
    else if (index == 0) { prepend(value) }
    else {
      val throwIndexTooLarge = () => throw new IndexOutOfBoundsException(
        s"Cannot insert at index $index as existing list is too short"
      )
      head match {
        case None => throwIndexTooLarge()
        case Some(node) =>
          var beforeNode = node
          (1 until index).foreach { _ => beforeNode = beforeNode.next.getOrElse(throwIndexTooLarge()) }
          val newNode = Some(DoublyLinkedNode(value, beforeNode.next, Some(beforeNode)))
          beforeNode.next match {
            case Some(node) => node.prev = newNode
            case None => tail = newNode
          }
          beforeNode.next = newNode
      }
    }
  }

}

object DoublyLinkedList {

  /***
   * Create a doubly-linked list from a Scala list in O(n).
   *
   * @param list Scala list to take values from.
   * @tparam T Type of the values.
   * @return
   */
  def fromList[T](list: List[T]): DoublyLinkedList[T] = {
    val nodes = list.map(DoublyLinkedNode(_))
    val linkedList = DoublyLinkedList(nodes.headOption, nodes.headOption)
    (1 until nodes.length).foreach { i =>
      nodes(i-1).next = Some(nodes(i))
      nodes(i).prev = Some(nodes(i-1))
      if (i == (nodes.length - 1)) {
        linkedList.tail = Some(nodes(i))
      }
    }
    linkedList
  }

}
