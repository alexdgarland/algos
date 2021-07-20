package crackingthecodinginterview

import scala.collection.mutable.ListBuffer

package object linkedlists {

  case class Node
  (
  value: Int,
  var next: Option[Node] = None
//  var prev: Option[Node] = None
  )

  case class LinkedList
  (
  head: Option[Node]
  ) {
    /***
     * Converter method mainly to make testing easier.
     *
     * @return
     */
    def toList: List[Int] = {
      val listBuffer = ListBuffer[Int]()
      var currentNode = head
      while(currentNode.isDefined) {
        listBuffer.append(currentNode.get.value)
        currentNode = currentNode.flatMap(_.next)
      }
      listBuffer.toList
    }
  }

  object LinkedList {

    /***
     * Converter method mainly to make testing easier.
     *
     * @param list Scala list to convert to a linked list
     * @return
     */
    def fromList(list: List[Int]): LinkedList = {
      val linkedList = LinkedList(list.headOption.map(Node(_)))

      linkedList.head match {
        case Some(value) =>
          var latestAttachedNode = Some(value)
          list.tail.foreach { value =>
            val newNode = Some(Node(value))
            latestAttachedNode.get.next = newNode
            latestAttachedNode = newNode
          }
        case None =>
      }
      linkedList
    }

  }

}
