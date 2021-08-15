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
    new ListMapper[T, DoublyLinkedNode[T], DoublyLinkedList[T], TT, DoublyLinkedNode[TT], DoublyLinkedList[TT]]() {
      override def newList(): DoublyLinkedList[TT] = DoublyLinkedList()
      override def newNode(value: TT, previousNode: Option[DoublyLinkedNode[TT]]): DoublyLinkedNode[TT] =
        DoublyLinkedNode(value, None, previousNode)
    }.map(this, f)
  }

  override protected def deleteNextNode(beforeNode: DoublyLinkedNode[T]): Unit = {
    beforeNode.next = beforeNode.next.get.next
    beforeNode.next.foreach(_.prev = Some(beforeNode))
  }

  override protected def initialAssignForDeleteWhere(firstRetainedNodeOption: Option[DoublyLinkedNode[T]]): Unit = {
    firstRetainedNodeOption.foreach(_.prev = None)
    head = firstRetainedNodeOption
    tail = firstRetainedNodeOption
  }

  override protected def insertAfter(value: T, beforeNode: DoublyLinkedNode[T]): Unit = {
    val newNode = Some(DoublyLinkedNode(value, beforeNode.next, Some(beforeNode)))
    beforeNode.next match {
      case Some(node) => node.prev = newNode
      case None => tail = newNode
    }
    beforeNode.next = newNode
  }

  override private[linkedlists] def setToTail(node: Option[DoublyLinkedNode[T]]): Unit = tail = node

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
