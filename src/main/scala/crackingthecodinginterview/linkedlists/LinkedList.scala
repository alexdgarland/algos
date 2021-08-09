package crackingthecodinginterview.linkedlists

import scala.collection.mutable.ListBuffer

trait LinkedList[T, N <: ListNode[T, _], +LL] {

  var head: Option[N]

  /** *
   * Converter method mainly to make testing easier.
   *
   * @return
   */
  def toList
  (
    start: Option[N] = head,
    move: N => Option[N] = node => node.next.asInstanceOf[Option[N]]
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
   * @param value
   */
  def append(value: T): Unit

  /***
   * Delete all nodes from linked list where values meet the supplied predicate.
   *
   * Runs in O(n) as we must check all nodes in the list.
   *
   * @param predicate Function to evaluate whether a node should be removed based on its value.
   */
  def deleteWhere(predicate: T => Boolean): Unit

  def map[TT](f: T => TT): LL

}

/**
 * Abstract class to handle shared implementation of deleteWhere.
 *
 * @param list List of type L to delete elements from.
 * @tparam T Type of values.
 * @tparam NN Type of list nodes (singly- or doubly-linked, plus value type).
 * @tparam L Type of list (singly- or doubly-linked, plus value type).
 */
abstract class PredicateBasedNodeDeleter[T, NN <: ListNode[T, _], L <: LinkedList[T, NN, _]](protected val list: L) {

  protected def initialAssign(firstRetainedNodeOption: Option[NN]): Unit

  protected def skipNode(nodeBeforeSkippable: NN): Unit

  protected def assignTail(potentialTailNode: NN): Unit

  def deleteWhere(predicate: T => Boolean): Unit = {
    list.head match {
      case None =>
      // (Do nothing when dealing with an empty list)
      case Some(node) =>
        // Fast-forward through list to find the first non-deletable item to serve as new head
        // If none are found the list will be completely emptied out without having to do any repointing
        var currentNodeOption: Option[NN] = Some(node)
        while (currentNodeOption.exists(n => predicate(n.value))) {
          currentNodeOption = currentNodeOption.flatMap(_.next.asInstanceOf[Option[NN]])
        }
        initialAssign(currentNodeOption)
        // If we still have nodes left, repoint as needed
        while(currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if (currentNode.next.exists(n => predicate(n.asInstanceOf[NN].value))) {
            skipNode(currentNode)
          }
          assignTail(currentNode)
          currentNodeOption = currentNode.next.asInstanceOf[Option[NN]]
        }
    }
  }

}
