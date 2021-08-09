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

  /***
   * Insert a value to an arbitrary point in the list represented by a supplied index.
   *
   * Runs in O(i) where i is the value of the requested index -
   * the largest value of that that can work is the size of the list at which point it's O(n)
   * (either way it's linear).
   *
   * Inserting to the start of the list (index 0) is constant-time
   * and as this is one of the main reasons to use a linked list, will expose it as a separate method as well.
   *
   * @param value The value to insert.
   * @param index The index at which to insert. If negative or too large for the list, an error will be thrown.
   */
  def insertAt(value: T, index: Int): Unit = {
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
          beforeNode.next = Some(SinglyLinkedNode(value, beforeNode.next))
      }
    }
  }

  def prepend(value: T): Unit = head = Some(SinglyLinkedNode(value, head))

  /***
   * Delete one element from the list at a given index.
   *
   * Runs in O(i) where i is the value of the requested index -
   * the largest value of that that can work is the size of the list at which point it's O(n)
   * (either way it's linear).
   *
   * @param index The index at which to delete a node.
   */
  def deleteAt(index: Int): Unit = {
    val throwIndexTooLarge = () => throw new IndexOutOfBoundsException(
      s"Cannot delete element at index $index as this is beyond the end of the linked list"
    )
    head match {
      case None =>
        throw new IndexOutOfBoundsException("Cannot delete from an empty linked list")
      case Some(node) =>
        var beforeNode = node
        (1 until index).foreach { _ =>
          beforeNode = beforeNode.next.getOrElse(throwIndexTooLarge())
        }
        beforeNode.next match {
          case None =>
            throwIndexTooLarge()
          case Some(nodeToDelete) =>
            beforeNode.next = nodeToDelete.next
        }
    }
  }

  private val whereDeleter = new PredicateBasedNodeDeleter[T, SinglyLinkedNode[T], SinglyLinkedList[T]](this) {
    override protected def initialAssign(firstRetainedNodeOption: Option[SinglyLinkedNode[T]]): Unit =
      list.head = firstRetainedNodeOption

    override protected def skipNode(nodeBeforeSkippable: SinglyLinkedNode[T]): Unit =
      nodeBeforeSkippable.next = nodeBeforeSkippable.next.get.next

    override protected def assignTail(potentialTailNode: SinglyLinkedNode[T]): Unit = ()
  }

  def deleteWhere(predicate: T => Boolean): Unit = whereDeleter.deleteWhere(predicate)

  def map[TT](f: T => TT): SinglyLinkedList[TT] = {
    val mappedList = SinglyLinkedList[TT]()
    head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val mappedHeadNode = SinglyLinkedNode(f(headNode.value))
        mappedList.head = Some(mappedHeadNode)
        var currentSourceNode: Option[SinglyLinkedNode[T]] = headNode.next
        var latestAttachedMappedNode: SinglyLinkedNode[TT] = mappedHeadNode
        while(currentSourceNode.isDefined) {
          val nextMappedNode = SinglyLinkedNode(f(currentSourceNode.get.value))
          latestAttachedMappedNode.next = Some(nextMappedNode)
          latestAttachedMappedNode = nextMappedNode
          currentSourceNode = currentSourceNode.get.next
        }
    }
    mappedList
  }

  /***
   * Remove all duplicate values from the list in O(n), keeping track of what values we've already seen using a map.
   *
   * The map adds worst-case space complexity of O(n), it will be smaller in as far as there are duplicate values.
   */
  def deduplicate(): Unit = {
    head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val seenValues = MutableMap[T, Boolean]()
        var previousNodeOption: Option[SinglyLinkedNode[T]] = Some(headNode)
        // Loop through each element in the list (so linear-time)
        while(previousNodeOption.flatMap(_.next).isDefined) {
          val previousNode = previousNodeOption.get
          val nextNode = previousNode.next.get
          seenValues.put(previousNode.value, true)
          // Either way this if statement branches, we advance one position linearly towards the end
          if (seenValues.contains(nextNode.value)) {
            // Remove a node (keep previous the same - the end still gets closer!)
            previousNode.next = nextNode.next
          }
          else {
            // If we're not removing a node, advance the "previous" node so we keep moving forward
            previousNodeOption = previousNode.next
          }
        }
    }
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
