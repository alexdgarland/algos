package crackingthecodinginterview.linkedlists

import scala.collection.mutable.{ListBuffer, Map => MutableMap}

case class LinkedList[T](var head: Option[SinglyLinkedNode[T]] = None) {
  /** *
   * Converter method mainly to make testing easier.
   *
   * @return
   */
  def toList: List[T] = {
    val listBuffer = ListBuffer[T]()
    var currentNode = head
    while (currentNode.isDefined) {
      listBuffer.append(currentNode.get.value)
      currentNode = currentNode.flatMap(_.next)
    }
    listBuffer.toList
  }

  /** *
   * Append a new value to the end of the linked-list.
   *
   * Runs in O(n) where n is the length of the existing list, as we have to touch each element on the way through.
   *
   * @param value The value to insert.
   */
  def append(value: T): Unit = {
    val newNode = SinglyLinkedNode(value)
    head match {
      case None =>
        head = Some(newNode)
      case Some(existingNode) =>
        var currentNode = existingNode
        // Iterate through to the last node -
        // this pattern seems like it might benefit from being abstracted out (recursively?) at some point
        while (currentNode.next.isDefined) {
          currentNode = currentNode.next.get
        }
        currentNode.next = Some(newNode)
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
    val newNode = SinglyLinkedNode(value)
    if (index < 0) {
      throw new IndexOutOfBoundsException("Cannot insert to a negative index")
    }
    else if (index == 0) {
      newNode.next = head
      head = Some(newNode)
    }
    else {
      val throwIndexTooLarge = () =>
        throw new IndexOutOfBoundsException(s"Cannot insert at index $index as existing list is too short")
      head match {
        case None =>
          throwIndexTooLarge()
        case Some(node) =>
          var beforeNode = node
          (1 until index).foreach { _ =>
            beforeNode = beforeNode.next.getOrElse(throwIndexTooLarge())
          }
          newNode.next = beforeNode.next
          beforeNode.next = Some(newNode)
      }
    }
  }

  /***
   * Insert to the start of the list.
   *
   * This runs in constant time so should be default way to add data to linked list if ordering is not critical.
   *
   * @param value The value to insert.
   */
  def prepend(value: T): Unit = insertAt(value, 0)

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

  /***
   * Delete all nodes from linked list where values meet the supplied predicate.
   *
   * Runs in O(n) as we must check all nodes in the list.
   *
   * @param predicate Function to evaluate whether a node should be removed based on its value.
   */
  def deleteWhere(predicate: T => Boolean): Unit = {
    head match {
      case None =>
      // (Do nothing when dealing with an empty list)
      case Some(node) =>
        // Fast-forward through list to find the first non-deletable item to serve as new head
        // If none are found the list will be completely emptied out without having to do any repointing
        var currentNodeOption: Option[SinglyLinkedNode[T]] = Some(node)
        while (currentNodeOption.exists(n => predicate(n.value))) {
          currentNodeOption = currentNodeOption.flatMap(_.next)
        }
        head = currentNodeOption
        // If we still have nodes left, repoint as needed
        while(currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if(currentNode.next.exists(n => predicate(n.value))) {
            currentNode.next = currentNode.next.get.next
          }
          currentNodeOption = currentNode.next
        }
    }
  }

  def map[TT](f: T => TT): LinkedList[TT] = {
    val mappedList = LinkedList[TT]()
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

object LinkedList {

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
  def fromList[T](list: List[T]): LinkedList[T] = {
    val nodes = list.map(SinglyLinkedNode(_))
    (1 until nodes.length).foreach { i => nodes(i-1).next = Some(nodes(i)) }
    LinkedList(nodes.headOption)
  }

}
