package crackingthecodinginterview.linkedlists

import scala.collection.mutable.{Map => MutableMap}

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

  def deleteWhere(predicate: T => Boolean): Unit = {
    new PredicateBasedNodeDeleter[T, SinglyLinkedNode[T], SinglyLinkedList[T]](this) {
      override protected def initialAssign(firstRetainedNodeOption: Option[SinglyLinkedNode[T]]): Unit =
        list.head = firstRetainedNodeOption
      override protected def skipNode(nodeBeforeSkippable: SinglyLinkedNode[T]): Unit =
        nodeBeforeSkippable.next = nodeBeforeSkippable.next.get.next
      override protected def assignTail(potentialTailNode: SinglyLinkedNode[T]): Unit = ()
    }.deleteWhere(predicate)
  }

  def map[TT](f: T => TT): SinglyLinkedList[TT] = {
    new ListMapper[T, SinglyLinkedNode[T], SinglyLinkedList[T], TT, SinglyLinkedNode[TT], SinglyLinkedList[TT]](this) {
      override protected def newList(): SinglyLinkedList[TT] = SinglyLinkedList()
      override protected def newNode(value: TT, previousNode: Option[SinglyLinkedNode[TT]]): SinglyLinkedNode[TT] =
        SinglyLinkedNode(value)
      override protected def assignTail(node: SinglyLinkedNode[TT]): Unit = ()
    }.map(f)
  }

  def insertAt(value: T, index: Int): Unit = {
    new PositionalListInserter[T, SinglyLinkedNode[T], SinglyLinkedList[T]](this) {
      override protected def insertValue(value: T, beforeNode: SinglyLinkedNode[T]): Unit = {
        beforeNode.next = Some(SinglyLinkedNode(value, beforeNode.next))
      }
    }.insertAt(value, index)
  }

  def deleteAt(index: Int): Unit = {
    new PositionalListNodeDeleter[T, SinglyLinkedNode[T], SinglyLinkedList[T]](this) {
      override protected def deleteNode(beforeNode: SinglyLinkedNode[T], nodeToDelete: SinglyLinkedNode[T]): Unit = {
        beforeNode.next = nodeToDelete.next
      }
    }.deleteAt(index)
  }

  def deduplicate(): Unit = {
    new ListDeduplicator[T, SinglyLinkedNode[T], SinglyLinkedList[T]](this) {
      override protected def deleteNode(beforeNode: SinglyLinkedNode[T], nodeToDelete: SinglyLinkedNode[T]): Unit = {
        beforeNode.next = nodeToDelete.next
      }
    }.deduplicate()
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
  def fromList[T](list: List[T]): SinglyLinkedList[T] = {
    val nodes = list.map(SinglyLinkedNode(_))
    (1 until nodes.length).foreach { i => nodes(i-1).next = Some(nodes(i)) }
    SinglyLinkedList(nodes.headOption)
  }

}
