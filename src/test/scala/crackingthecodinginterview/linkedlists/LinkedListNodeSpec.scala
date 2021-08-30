package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class LinkedListNodeSpec extends AnyFlatSpec with Matchers {

  "DoublyLinkedNode" should "have expected (non-recursive) toString to avoid stack overflow" in {
    val list = DoublyLinkedList.fromList(List(1, 2, 3))
    val node1 = list.head.get
    node1.toString should be ("DoublyLinkedNode(1, [previous node missing], [next node present])")
    val node2 = node1.next.get
    node2.toString should be ("DoublyLinkedNode(2, [previous node present], [next node present])")
    val node3 = node2.next.get
    node3.toString should be ("DoublyLinkedNode(3, [previous node present], [next node missing])")
  }

  "SinglyLinkedNode" should "have expected (non-recursive) toString for brevity and consistency with DoublyLinkedNode" in {
    val list = SinglyLinkedList.fromList(List(1, 2, 3))
    val node1 = list.head.get
    node1.toString should be ("SinglyLinkedNode(1, [next node present])")
    val node2 = node1.next.get
    node2.toString should be ("SinglyLinkedNode(2, [next node present])")
    val node3 = node2.next.get
    node3.toString should be ("SinglyLinkedNode(3, [next node missing])")
  }

}
