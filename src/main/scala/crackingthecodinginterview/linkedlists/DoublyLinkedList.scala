package crackingthecodinginterview.linkedlists

import scala.collection.mutable.{Map => MutableMap}

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
    new PositionalListInserter[T, DoublyLinkedNode[T], DoublyLinkedList[T]](this) {
      override def insertValue(value: T, beforeNode: DoublyLinkedNode[T]): Unit = {
        val newNode = Some(DoublyLinkedNode(value, beforeNode.next, Some(beforeNode)))
        beforeNode.next match {
          case Some(node) => node.prev = newNode
          case None => tail = newNode
        }
        beforeNode.next = newNode
      }
    }.insertAt(value, index)
  }

  override def deleteAt(index: Int): Unit = {
    new PositionalListNodeDeleter[T, DoublyLinkedNode[T], DoublyLinkedList[T]](this) {
      override def deleteNode(beforeNode: DoublyLinkedNode[T], nodeToDelete: DoublyLinkedNode[T]): Unit = {
        beforeNode.next = nodeToDelete.next
        beforeNode.next.foreach(_.prev = Some(beforeNode))
      }
    }.deleteAt(index)
  }

  override def deduplicate(): Unit = {
    head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val seenValues = MutableMap[T, Boolean]()
        var previousNodeOption: Option[DoublyLinkedNode[T]] = Some(headNode)
        // Loop through each element in the list (so linear-time)
        while(previousNodeOption.flatMap(_.next).isDefined) {
          val previousNode = previousNodeOption.get
          val nextNode = previousNode.next.get
          seenValues.put(previousNode.value, true)
          // Either way this if statement branches, we advance one position linearly towards the end
          if (seenValues.contains(nextNode.value)) {
            // Remove a node (keep previous the same - the end still gets closer!)
            val newNextOption = nextNode.next
            previousNode.next = newNextOption
            newNextOption.get.prev = Some(previousNode)
          }
          else {
            // If we're not removing a node, advance the "previous" node so we keep moving forward
            previousNodeOption = previousNode.next
          }
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
