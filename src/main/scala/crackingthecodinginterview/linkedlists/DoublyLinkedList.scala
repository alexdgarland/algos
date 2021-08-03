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
) extends LinkedList[T, DoublyLinkedNode[T]] {

  def toListReversed: List[T] = toList(tail, node => node.prev)

  def deleteWhere(predicate: T => Boolean): Unit = {
    val initialAssigner = (currentNodeOption: Option[DoublyLinkedNode[T]]) => {
      currentNodeOption.foreach(_.prev = None)
      head = currentNodeOption
      tail = currentNodeOption
    }
    val nodeSkipper = (currentNode: DoublyLinkedNode[T]) => {
      val skipToNode = currentNode.next.get.next
      currentNode.next = skipToNode
      skipToNode match {
        case Some(node) =>
          node.prev = Some(currentNode)
        case None =>
      }
    }
    val tailAssigner = (currentNode: DoublyLinkedNode[T]) => currentNode.next match {
      case Some(node) => tail = Some(node)
      case None =>
    }
    LinkedList.deleteWhere(this, predicate, initialAssigner, nodeSkipper, tailAssigner)
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
