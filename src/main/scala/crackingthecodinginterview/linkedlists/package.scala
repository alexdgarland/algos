package crackingthecodinginterview

import scala.collection.mutable.ListBuffer

package object linkedlists {

  case class Node
  (
    value: Int,
    var next: Option[Node] = None
    //  var prev: Option[Node] = None
  )

  case class LinkedList(var head: Option[Node] = None) {
    /** *
     * Converter method mainly to make testing easier.
     *
     * @return
     */
    def toList: List[Int] = {
      val listBuffer = ListBuffer[Int]()
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
    def append(value: Int): Unit = {
      val newNode = Node(value)
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
    def insert(value: Int, index: Int): Unit = {
      val newNode = Node(value)
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
    def prepend(value: Int): Unit = insert(value, 0)

    /***
     * Delete one element from the list at a given index.
     *
     * Runs in O(i) where i is the value of the requested index -
     * the largest value of that that can work is the size of the list at which point it's O(n)
     * (either way it's linear).
     *
     * @param index The index at which to delete a node.
     */
    def delete(index: Int): Unit = {
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
    def fromList(list: List[Int]): LinkedList = {
      val nodes = list.map(Node(_))
      (1 until nodes.length).foreach { i => nodes(i-1).next = Some(nodes(i)) }
      LinkedList(nodes.headOption)
    }

  }

}
