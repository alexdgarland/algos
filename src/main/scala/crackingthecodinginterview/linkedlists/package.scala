package crackingthecodinginterview

package object linkedlists {

  // To understand roughly what I'm trying to do here (and maybe improve it in future!) this is useful:
  // https://github.com/ghik/opinionated-scala/blob/master/chapters/Generics-and-type-members.md#f-bounded-polymorphism

  sealed trait ListNode[T, Self <: ListNode[T, Self]] {
    var value: T
    var next: Option[Self]
  }

  case class SinglyLinkedNode[T]
  (
    var value: T,
    var next: Option[SinglyLinkedNode[T]] = None
  ) extends ListNode[T, SinglyLinkedNode[T]]

  case class DoublyLinkedNode[T]
  (
    var value: T,
    var next: Option[DoublyLinkedNode[T]] = None,
    var prev: Option[DoublyLinkedNode[T]] = None
  ) extends ListNode[T, DoublyLinkedNode[T]]

}
