package crackingthecodinginterview.linkedlists

import scala.collection.mutable.ListBuffer

/***
 *
 * @param head First item in the list (may be null if list is empty).
 * @param tail Last item in the list (may be null if list is empty).
 * @tparam T Types of the values.
 */
case class DoublyLinkedList[T]
(
  var head: Option[DoublyLinkedNode[T]] = None,
  var tail: Option[DoublyLinkedNode[T]] = None
) {

  private def toList
  (
    start: Option[DoublyLinkedNode[T]],
    move: DoublyLinkedNode[T] => Option[DoublyLinkedNode[T]]
  ): List[T] = {
    val listBuffer = ListBuffer[T]()
    var currentNode = start
    while (currentNode.isDefined) {
      listBuffer.append(currentNode.get.value)
      currentNode = currentNode.flatMap(move(_))
    }
    listBuffer.toList
  }

  def toListForward: List[T] = toList(head, node => node.next)

  def toListReversed: List[T] = toList(tail, node => node.prev)

  /***
   * Delete all nodes from linked list where values meet the supplied predicate.
   *
   * Runs in O(n) as we must check all nodes in the list.
   *
   * @param predicate Function to evaluate whether a node should be removed based on its value.
   */
  def deleteWhere(predicate: T => Boolean): Unit = {
    // TODO - this duplicates quite a lot of code from the singly-linked version (with some additions)
    //  so at some point might be worth seeing how much can be factored out to shared.
    head match {
      case None =>
      // (Do nothing when dealing with an empty list)
      case Some(node) =>
        // Fast-forward through list to find the first non-deletable item to serve as new head
        // If none are found the list will be completely emptied out without having to do any repointing
        var currentNodeOption: Option[DoublyLinkedNode[T]] = Some(node)
        while (currentNodeOption.exists(n => predicate(n.value))) {
          currentNodeOption = currentNodeOption.flatMap(_.next)
        }
        head = currentNodeOption
        tail = currentNodeOption
        currentNodeOption.foreach(_.prev = None)
        // If we still have nodes left, repoint as needed
        while(currentNodeOption.isDefined) {
          val currentNode = currentNodeOption.get
          if(currentNode.next.exists(n => predicate(n.value))) {
            val skipToNode = currentNode.next.get.next
            currentNode.next = skipToNode
            skipToNode match {
              case Some(node) =>
                node.prev = Some(currentNode)
              case None =>
            }
          }
          currentNode.next match {
            case Some(node) => tail = Some(node)
            case None =>
          }
          currentNodeOption = currentNode.next
        }
    }
  }

}

object DoublyLinkedList {

  /***
   * Create a doubly-linked list from a Scala list in O(n).
   *
   * @param list Scala list to take values from.
   * @tparam T Type of the values.
   * @return
   */
  def fromList[T](list: List[T]): DoublyLinkedList[T] = {
    val nodes = list.map(DoublyLinkedNode(_))
    val linkedList = DoublyLinkedList(nodes.headOption, nodes.headOption)
    (1 until nodes.length).foreach { i =>
      nodes(i-1).next = Some(nodes(i))
      nodes(i).prev = Some(nodes(i-1))
      if (i == (nodes.length - 1)) {
        linkedList.tail = Some(nodes(i))
      }
    }
    linkedList
  }

}
