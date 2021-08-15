package crackingthecodinginterview.linkedlists

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MutableMap}

trait LinkedList[T, N <: ListNode[T, N], +LL] {

  var head: Option[N]

  /** *
   * Converter method mainly to make testing easier.
   *
   * @return
   */
  def toList
  (
    start: Option[N] = head,
    move: N => Option[N] = node => node.next
  ): List[T] = {
    val listBuffer = ListBuffer[T]()
    var currentNode = start
    while (currentNode.isDefined) {
      listBuffer.append(currentNode.get.value)
      currentNode = currentNode.flatMap(move(_))
    }
    listBuffer.toList
  }

  /***
   * Insert to the start of the list.
   *
   * This runs in constant time so should be default way to add data to linked list if ordering is not critical.
   *
   * @param value The value to insert.
   */
  def prepend(value: T): Unit

  /***
   * Insert to end of list. Time complexity varies between implementations.
   *
   * @param value Value to append.
   */
  def append(value: T): Unit

  protected def deleteNextNode(beforeNode: N): Unit

  protected def initialAssignForDeleteWhere(firstRetainedNodeOption: Option[N]): Unit

  protected def assignTailForDeleteWhere(potentialTailNode: N): Unit

  protected def insertAfter(value: T, beforeNode: N): Unit

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
        var currentNodeOption: Option[N] = Some(node)
        while (currentNodeOption.exists(n => predicate(n.value))) {
          currentNodeOption = currentNodeOption.flatMap(_.next)
        }
        initialAssignForDeleteWhere(currentNodeOption)
        // If we still have nodes left, repoint as needed
        while(currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if (currentNode.next.exists(n => predicate(n.value))) {
            deleteNextNode(currentNode)
          }
          assignTailForDeleteWhere(currentNode)
          currentNodeOption = currentNode.next
        }
    }
  }

  def map[TT](f: T => TT): LL

  /***
   * Insert a value to an arbitrary point in the list represented by a supplied index.
   *
   * Runs in O(i) where i is the value of the requested index -
   * the largest value of that that can work is the size of the list at which point it's O(n)
   * (either way it's linear).
   *
   * Inserting to the start of the list (index 0) is constant-time and as this is one of the main reasons
   * to use a linked list, this is explicitly provided as a separate prepend method.
   *
   * @param value The value to insert.
   * @param index The index at which to insert. If negative or too large for the list, an error will be thrown.
   */
  def insertAt(value: T, index: Int): Unit  = {
    if (index < 0) {
      throw new IndexOutOfBoundsException("Cannot insert to a negative index")
    }
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
          insertAfter(value, beforeNode)
      }
    }
  }

  /***
   * Delete one element from the list at a given index.
   *
   * Runs in O(i) where i is the value of the requested index -
   * the largest value of that that can work is the size of the list at which point it's O(n)
   * (either way it's linear).
   *
   * @param index The index at which to delete a node.
   */
  def deleteAt(index: Int) : Unit = {
    val throwIndexTooLarge = () => throw new IndexOutOfBoundsException(
      s"Cannot delete element at index $index as this is beyond the end of the linked list"
    )
    head match {
      case None =>
        throw new IndexOutOfBoundsException("Cannot delete from an empty linked list")
      case Some(node) =>
        var beforeNode = node
        (1 until index).foreach { _ => beforeNode = beforeNode.next.getOrElse(throwIndexTooLarge()) }
        beforeNode.next match {
          case None =>
            throwIndexTooLarge()
          case Some(nodeToDelete) =>
            deleteNextNode(beforeNode)
        }
    }
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
        var previousNodeOption: Option[N] = Some(headNode)
        // Loop through each element in the list (so linear-time)
        while(previousNodeOption.flatMap(_.next).isDefined) {
          val previousNode = previousNodeOption.get
          val nextNode = previousNode.next.get
          seenValues.put(previousNode.value, true)
          // Either way this if statement branches, we advance one position linearly towards the end
          if (seenValues.contains(nextNode.value)) {
            // Remove a node (keep previous the same - the end still gets closer!)
            deleteNextNode(previousNode)
          }
          else {
            // If we're not removing a node, advance the "previous" node so we keep moving forward
            previousNodeOption = previousNode.next
          }
        }
    }
  }

}

private[linkedlists] abstract class ListMapper[
  T, N <: ListNode[T, N], L <: LinkedList[T, N, _], TT, NN <: ListNode[TT, NN], LL <: LinkedList[TT, NN, _]
](protected val list: L) {

  protected val mappedList: LL = newList()

  protected def newList(): LL

  protected def newNode(value: TT, previousNode: Option[NN] = None): NN

  protected def assignTail(node: NN): Unit

  def map(f: T => TT): LL = {
    list.head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val mappedHeadNode = newNode(f(headNode.value))
        mappedList.head = Some(mappedHeadNode)
        var currentSourceNode: Option[N] = headNode.next
        var latestAttachedMapNode: NN = mappedHeadNode
        while(currentSourceNode.isDefined) {
          val nextMappedNode = newNode(f(currentSourceNode.get.value), Some(latestAttachedMapNode))
          latestAttachedMapNode.next = Some(nextMappedNode)
          latestAttachedMapNode = nextMappedNode
          currentSourceNode = currentSourceNode.get.next
        }
        assignTail(latestAttachedMapNode)
    }
    mappedList
  }

}
