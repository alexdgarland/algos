package crackingthecodinginterview

package object linkedlists {

  case class SinglyLinkedNode[T]
  (
    value: T,
    var next: Option[SinglyLinkedNode[T]] = None
  )

}
