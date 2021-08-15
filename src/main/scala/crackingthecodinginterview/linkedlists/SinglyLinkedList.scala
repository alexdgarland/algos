package crackingthecodinginterview.linkedlists

case class SinglyLinkedList[T](var head: Option[SinglyLinkedNode[T]] = None) extends LinkedList[T, SinglyLinkedNode[T], SinglyLinkedList[_]] {

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

  def map[TT](f: T => TT): SinglyLinkedList[TT] = {
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
  def fromList[T](list: List[T]): SinglyLinkedList[T] = {
    val nodes = list.map(SinglyLinkedNode(_))
    (1 until nodes.length).foreach { i => nodes(i-1).next = Some(nodes(i)) }
    SinglyLinkedList(nodes.headOption)
  }

}
