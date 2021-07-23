package crackingthecodinginterview

package object linkedlists {

  case class SinglyLinkedNode[T]
  (
    value: T,
    var next: Option[SinglyLinkedNode[T]] = None
  )

  case class DoublyLinkedNode[T]
  (
    value: T,
    var next: Option[DoublyLinkedNode[T]] = None,
    var prev: Option[DoublyLinkedNode[T]] = None
  )

}
