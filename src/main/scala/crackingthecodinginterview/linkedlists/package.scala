package crackingthecodinginterview

package object linkedlists {

  // To understand roughly what I'm trying to do here (and maybe improve it in future!) this is useful:
  // https://github.com/ghik/opinionated-scala/blob/master/chapters/Generics-and-type-members.md#f-bounded-polymorphism

  sealed trait ListNode[T, Self <: ListNode[T, Self]] {
    var value: T
    var next: Option[Self]

    protected final def optStr(nodeOpt: Option[_], desc: String) =
      s"[$desc node ${if (nodeOpt.isDefined) "present" else "missing"}]"
  }

  // This needs to be a class NOT a case class so we can do a reference equality check for the intersect method
  class SinglyLinkedNode[T](var value: T, var next: Option[SinglyLinkedNode[T]])
    extends ListNode[T, SinglyLinkedNode[T]] {
    override def toString: String = s"SinglyLinkedNode($value, ${optStr(next, "next")})"
  }

  object SinglyLinkedNode {
    def apply[T](value: T, next: Option[SinglyLinkedNode[T]] = None): SinglyLinkedNode[T] = {
      new SinglyLinkedNode[T](value, next)
    }
  }

  case class DoublyLinkedNode[T]
  (
    var value: T,
    var next: Option[DoublyLinkedNode[T]] = None,
    var prev: Option[DoublyLinkedNode[T]] = None
  ) extends ListNode[T, DoublyLinkedNode[T]] {
    override def toString: String = {
      s"DoublyLinkedNode($value, ${optStr(prev, "previous")}, ${optStr(next, "next")})"
    }
  }

}
