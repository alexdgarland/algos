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

object LinkedList {

  /***
   *
   * Shared static function to delete all nodes matching a predicate from any (singly- or doubly-) linked list.
   *
   * Additional function parameters allow passing the differing behaviour for singly- vs doubly-linked lists.
   *
   * TODO - might be cleaner to turn this into a trait with the passed functions attached as method impls?
   *
   * Runs in O(n) in both cases, as we must check all nodes in the list.
   *
   * @param linkedList The list to delete nodes from.
   * @param predicate Function to evaluate whether a node should be removed based on its value.
   * @param initialAssigner The action to do once we find the first node to keep (does not match predicate).
   * @param nodeSkipper Action to take when we find a node that matches the predicate and should not be kept in list.
   * @param tailAssigner Action (no-op for singly-linked list)
   * @tparam T Node value type.
   * @tparam NN Type of node (singly or doubly linked, plus value type).
   * @tparam L Type of list (singly or doubly linked, plus value type).
   */
  private[linkedlists] def deleteWhere[T, NN <: ListNode[T, _], L <: LinkedList[T, NN]]
  (
    linkedList: L,
    predicate: T => Boolean,
    initialAssigner: Option[NN] => Unit,
    nodeSkipper: NN => Unit,
    tailAssigner: NN => Unit
  ): Unit = {
    linkedList.head match {
      case None =>
        // (Do nothing when dealing with an empty list)
      case Some(node) =>
        // Fast-forward through list to find the first non-deletable item to serve as new head
        // If none are found the list will be completely emptied out without having to do any repointing
        var currentNodeOption: Option[NN] = Some(node)
        while (currentNodeOption.exists(n => predicate(n.value))) {
          currentNodeOption = currentNodeOption.flatMap(_.next.asInstanceOf[Option[NN]])
        }
        initialAssigner(currentNodeOption)
        // If we still have nodes left, repoint as needed
        while(currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if (currentNode.next.exists(n => predicate(n.asInstanceOf[NN].value))) {
            nodeSkipper(currentNode)
          }
          tailAssigner(currentNode)
          currentNodeOption = currentNode.next.asInstanceOf[Option[NN]]
        }
    }
  }

}
