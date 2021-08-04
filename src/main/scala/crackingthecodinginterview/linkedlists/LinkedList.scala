package crackingthecodinginterview.linkedlists

import scala.collection.mutable.ListBuffer

trait LinkedList[T, N <: ListNode[T, _]] {

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
   * Delete all nodes from linked list where values meet the supplied predicate.
   *
   * Runs in O(n) as we must check all nodes in the list.
   *
   * @param predicate Function to evaluate whether a node should be removed based on its value.
   */
  def deleteWhere(predicate: T => Boolean): Unit

}

abstract class PredicateBasedNodeDeleter[T, NN <: ListNode[T, _], L <: LinkedList[T, NN]](protected val list: L) {

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
