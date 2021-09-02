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

  override def partition(partitionValue: T): Unit = {
    import ordering.mkOrderingOps
    val leftBuilder = SinglyLinkedList.ListBuilder()
    val rightBuilder = SinglyLinkedList.ListBuilder()
    var currentNodeOption: Option[SinglyLinkedNode[T]] = head
    while(currentNodeOption.isDefined) {
      val builder = if(currentNodeOption.exists(_.value < partitionValue)) leftBuilder else rightBuilder
      builder.addNodeOption(currentNodeOption)
      currentNodeOption = currentNodeOption.flatMap(_.next)
    }
    head = (leftBuilder + rightBuilder).build().head
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
    val builder = SinglyLinkedList.ListBuilder[Int]()
    var list1Node = list1.head
    var list2Node = list2.head
    var carry = false
    while(list1Node.isDefined || list2Node.isDefined) {
      val total = list1Node.map(_.value).getOrElse(0) + list2Node.map(_.value).getOrElse(0) + (if(carry) 1 else 0)
      carry = total > 9
      builder.addValue(total % 10)
      list1Node = list1Node.flatMap(_.next)
      list2Node = list2Node.flatMap(_.next)
    }
    if(carry) builder.addValue(1)
    builder.build()
  }

}
