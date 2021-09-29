package crackingthecodinginterview.linkedlists

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.{Map => MutableMap}

trait LinkedList[T, N <: ListNode[T, N], +LL] {

  var head: Option[N]

  /** *
   * Converter method mainly to make testing easier.
   *
   * @return
   */
  def toList(start: Option[N] = head, move: N => Option[N] = node => node.next): List[T] = {
    val listBuffer = ListBuffer[T]()
    var currentNode = start
    while (currentNode.isDefined) {
      listBuffer.append(currentNode.get.value)
      currentNode = currentNode.flatMap(move(_))
    }
    listBuffer.toList
  }

  /**
   * Return the length of the linked list.
   *
   * Currently calculates it each time the method is called, in O(n).
   *
   * Alternatively, could store the length (giving low-cost constant-time retrieval) at the cost of:
   *  - a very small (constant-time, 32-bit int) increase in space
   *  - having to make sure that the length is updated every time the list is mutated -
   *    this is an issue in particular because we have a deleteNode method which does not have access to the full list.
   *
   * @return length of the linked list
   */
  def length: Int = {
    @tailrec
    def inner(nextNode: Option[N], acc: Int): Int = {
      nextNode match {
        case None => acc
        case Some(node) => inner(node.next, acc + 1)
      }
    }

    inner(head, 0)
  }

  @tailrec
  protected final def moveToIndex(nextNode: Option[N], remainingIndex: Int, advance: N => Option[N]): Option[N] = {
    nextNode match {
      case None =>
        None
      case Some(node) =>
        if (remainingIndex < 0)
          None
        else if (remainingIndex == 0)
          Some(node)
        else
          moveToIndex(advance(node), remainingIndex - 1, advance)
    }
  }

  /**
   * Return a node from the list at a given index in O(n).
   *
   * @param index Index of node to retrieve
   * @return Option of node - if index is out of range None, otherwise Some(node)
   */
  def apply(index: Int): Option[N] = moveToIndex(head, index, _.next)

  /**
   * Get the kth-from-last node in the list (i.e. count backwards by k nodes from the final node).
   *
   * Implemented differently for each version of linked list.
   *
   * @param k reverse-index to retrieve node for
   * @return Option of node - if index is out of range None, otherwise Some(node)
   */
  def kthFromLast(k: Int): Option[N]

  /** *
   * Insert to the start of the list.
   *
   * This runs in constant time so should be default way to add data to linked list if ordering is not critical.
   *
   * @param value The value to insert.
   */
  def prepend(value: T): Unit

  /** *
   * Insert to end of list. Time complexity varies between implementations.
   *
   * @param value Value to append.
   */
  def append(value: T): Unit

  private[linkedlists] def setToTail(node: Option[N]): Unit

  protected def deleteNextNode(beforeNode: N): Unit

  protected def initialAssignForDeleteWhere(firstRetainedNodeOption: Option[N]): Unit

  protected def insertAfter(value: T, beforeNode: N): Unit

  /** *
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
        while (currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if (currentNode.next.exists(n => predicate(n.value))) {
            deleteNextNode(currentNode)
          }
          currentNode.next.foreach(node => setToTail(Some(node)))
          currentNodeOption = currentNode.next
        }
    }
  }

  def map[TT](f: T => TT): LL

  /** *
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
  def insertAt(value: T, index: Int): Unit = {
    if (index < 0) {
      throw new IndexOutOfBoundsException("Cannot insert to a negative index")
    }
    else if (index == 0) {
      prepend(value)
    }
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

  /** *
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
        (1 until index).foreach { _ => beforeNode = beforeNode.next.getOrElse(throwIndexTooLarge()) }
        beforeNode.next match {
          case None =>
            throwIndexTooLarge()
          case Some(_) =>
            deleteNextNode(beforeNode)
        }
    }
  }

  /** *
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
        while (previousNodeOption.flatMap(_.next).isDefined) {
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

  /**
   * Rearrange nodes in the list so that all nodes less than the partition value appear before
   * all nodes that are greater than or equal to the value.
   *
   * @param partitionValue Value around which to partition the list
   */
  def partition(partitionValue: T)(implicit ordering: Ordering[T]): Unit

  /**
   * Perform an in-place reversal of the nodes in the list.
   */
  def reverse(): Unit

  /**
   * Indicate whether a linked list is a palindrome (values of nodes are the same reverse as forward.
   *
   * @return
   */
  def isPalindrome: Boolean

  def forEachNode(f: N => Unit): Unit = {
    var currentNode = head
    while (currentNode.isDefined) {
      currentNode.foreach { node =>
        f(node)
        currentNode = node.next
      }
    }
  }

  def forEachValue(f: T => Unit): Unit = forEachNode(node => f(node.value))

  def foldLeftNodes[A](starterAcc: A)(f: (A, N) => A): A = {
    @tailrec
    def inner(nodeOption: Option[N], acc: A): A = nodeOption match {
      case None => acc
      case Some(node) => inner(node.next, f(acc, node))
    }

    inner(head, starterAcc)
  }

  def foldLeft[A](starterAcc: A)(f: (A, T) => A): A = foldLeftNodes(starterAcc)((acc, node) => f(acc, node.value))

  def findNode(predicate: N => Boolean): Option[N] = {
    @tailrec
    def inner(nodeOption: Option[N]): Option[N] = nodeOption match {
      case None => None
      case Some(node) => if (predicate(node)) Some(node) else inner(node.next)
    }

    inner(head)
  }

  def find(predicate: T => Boolean): Option[T] = findNode(node => predicate(node.value)).map(_.value)

}
