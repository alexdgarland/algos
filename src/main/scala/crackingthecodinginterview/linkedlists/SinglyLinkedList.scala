package crackingthecodinginterview.linkedlists

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
        while (currentNode.next.isDefined) { currentNode = currentNode.next.get }
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

  override private[linkedlists] def setToTail(node: Option[SinglyLinkedNode[T]]): Unit = { }

  /**
   * Implement for singly-linked list using existing methods for length and retrieve-by-index (apply), both of which run in O(n).
   *
   * As far as I can see, this is equivalent in complexity to the "runner pointer" solution from CTCI but makes more sense.
   *
   * @param k reverse-index to retrieve node for
   *  @return Option of node - if index is out of range None, otherwise Some(node)
   */
  override def kthFromLast(k: Int): Option[SinglyLinkedNode[T]] = this(this.length - (k + 1))

  private case class PartitionSubListBuilder
  (
    var head: Option[SinglyLinkedNode[T]] = None,
    var latest: Option[SinglyLinkedNode[T]] = None
  ) {
    def addNode(currentNodeOption: Option[SinglyLinkedNode[T]]): Unit = {
      if (head.isEmpty) head = currentNodeOption
      latest.foreach(_.next = currentNodeOption)
      latest = currentNodeOption
    }
  }

  override def partition(partitionValue: T): Unit = {
    import ordering.mkOrderingOps
    val leftBuilder = PartitionSubListBuilder()
    val rightBuilder = PartitionSubListBuilder()
    var currentNodeOption: Option[SinglyLinkedNode[T]] = head
    while(currentNodeOption.isDefined) {
      (if(currentNodeOption.get.value < partitionValue) leftBuilder else rightBuilder).addNode(currentNodeOption)
      currentNodeOption = currentNodeOption.get.next
    }
    rightBuilder.latest.foreach(_.next = None)
    leftBuilder.latest.foreach(_.next = rightBuilder.head)
    head = (if(leftBuilder.head.isEmpty) rightBuilder else leftBuilder).head
  }

}

object SinglyLinkedList {

  /** *
   * Converter method mainly to make testing easier.
   *
   * Runs in O(n) where n = length of list passed.
   *
   * NOT implemented using repeated calls to append() method as this itself runs in O(m)
   * (m being length of  linked-list built so far, averaging to n/ 2)
   * which would give a time complexity of O(n squared).
   *
   * @param list Scala list to convert to a linked list
   * @return
   */
  def fromList[T](list: List[T])(implicit ordering: Ordering[T]): SinglyLinkedList[T] = {
    val nodes = list.map(SinglyLinkedNode(_))
    (1 until nodes.length).foreach { i => nodes(i-1).next = Some(nodes(i)) }
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

}
