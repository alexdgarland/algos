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

  override def prepend(value: T): Unit = head = Some(DoublyLinkedNode(value, head))

  /***
   * Insert to end of list in constant time.
   *
   * @param value
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
    val mappedList = DoublyLinkedList[TT]()
    head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val mappedHeadNode = DoublyLinkedNode(f(headNode.value))
        mappedList.head = Some(mappedHeadNode)
        var currentSourceNode: Option[DoublyLinkedNode[T]] = headNode.next
        var latestAttachedMappedNode: DoublyLinkedNode[TT] = mappedHeadNode
        while(currentSourceNode.isDefined) {
          val nextMappedNode = DoublyLinkedNode(f(currentSourceNode.get.value), None, Some(latestAttachedMappedNode))
          latestAttachedMappedNode.next = Some(nextMappedNode)
          latestAttachedMappedNode = nextMappedNode
          currentSourceNode = currentSourceNode.get.next
        }
        mappedList.tail = Some(latestAttachedMappedNode)
    }
    mappedList
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
