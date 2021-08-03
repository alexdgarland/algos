package crackingthecodinginterview.linkedlists

import scala.collection.mutable.ListBuffer

trait LinkedList[T, NN <: ListNode[T, _]] {

  var head: Option[NN]

  /** *
   * Converter method mainly to make testing easier.
   *
   * @return
   */
  def toList
  (
    start: Option[NN] = head,
    move: NN => Option[NN] = node => node.next.asInstanceOf[Option[NN]]
  ): List[T] = {
    val listBuffer = ListBuffer[T]()
    var currentNode = start
    while (currentNode.isDefined) {
      listBuffer.append(currentNode.get.value)
      currentNode = currentNode.flatMap(move(_))
    }
    listBuffer.toList
  }

}
