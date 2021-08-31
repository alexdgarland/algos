package crackingthecodinginterview.linkedlists

import scala.annotation.tailrec

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

  /**
   * Doubly-linked list implementation runs backwards from the tail -
   * this is _very slightly_ more efficient in that it only does one O(n) pass (doesn't check the list length first)
   * but doesn't fundamentally change the time complexity.
   *
   * @param k reverse-index to retrieve node for
   *  @return Option of node - if index is out of range None, otherwise Some(node)
   */
  override def kthFromLast(k: Int): Option[DoublyLinkedNode[T]] = moveToIndex(tail, k, _.prev)

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

  /**
   * Deletes a middle (non-head/ non-tail) node from a linked-list given a reference to the node only
   * (NOT given a reference to list/ head/ parent node)
   *
   * @param node The node to delete
   * @tparam T Type of values within the node
   */
  def deleteNode[T](node: DoublyLinkedNode[T]): Unit = {
    (node.prev, node.next) match {
      case (None, None) =>
        // We don't have access to head or tail pointers,
        // so without additional nodes to mutate we can't do what is needed
        throw new IllegalArgumentException("Cannot delete only node in list without access to full list")
      case (Some(previousNode), Some(nextNode)) =>
        // If we're truly in the middle of the list, we can just repoint (no value changes within nodes)
        previousNode.next = Some(nextNode)
        nextNode.prev = Some(previousNode)
      case (Some(previousNode), None) =>
        // Deleting last node in list
        node.value = previousNode.value
        node.prev = previousNode.prev
        node.prev.foreach(_.next = Some(node))
      case (None, Some(nextNode)) =>
        // Deleting first node in list
        node.value = nextNode.value
        node.next = nextNode.next
        node.next.foreach(_.prev = Some(node))
    }
  }

}
