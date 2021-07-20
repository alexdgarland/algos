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
  var head: Option[Node] = None
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

    def append(value: Int): Unit = {
      val newNode = Node(value)
      head match {
        case None =>
          head = Some(newNode)
        case Some(existingNode) =>
          var currentNode = existingNode
          // Iterate through to the last node -
          // this pattern seems like it might benefit from being abstracted out (recursively?) at some point
          while(currentNode.next.isDefined) {
            currentNode = currentNode.next.get
          }
          currentNode.next = Some(newNode)
      }
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
      val linkedList = LinkedList()
      list.foreach { linkedList.append }
      linkedList
    }

  }

}
