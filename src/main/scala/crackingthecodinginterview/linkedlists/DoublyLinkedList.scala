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
) extends LinkedList[T, DoublyLinkedNode[T]] {

  def toListReversed: List[T] = toList(tail, node => node.prev)

  private val whereDeleter = new PredicateBasedNodeDeleter[T, DoublyLinkedNode[T], DoublyLinkedList[T]](this) {
    override protected def initialAssign(firstRetainedNodeOption: Option[DoublyLinkedNode[T]]): Unit = {
      firstRetainedNodeOption.foreach(_.prev = None)
      list.head = firstRetainedNodeOption
      list.tail = firstRetainedNodeOption
    }

    override protected def skipNode(nodeBeforeSkippable: DoublyLinkedNode[T]): Unit = {
      val skipToNode = nodeBeforeSkippable.next.get.next
      nodeBeforeSkippable.next = skipToNode
      skipToNode match {
        case Some(node) =>
          node.prev = Some(nodeBeforeSkippable)
        case None =>
      }
    }

    override protected def assignTail(potentialTailNode: DoublyLinkedNode[T]): Unit = {
      potentialTailNode.next match {
        case Some(node) => list.tail = Some(node)
        case None =>
      }
    }
  }

  def deleteWhere(predicate: T => Boolean): Unit = whereDeleter.deleteWhere(predicate)
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
