package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DoublyLinkedListSpec extends AnyFlatSpec with should.Matchers {

  "DoublyLinkedList" should "be convertible from and to a Scala list when populated" in {
    val scalaList = List(1, 2, 3, 4, 5, 6)
    DoublyLinkedList.fromList(scalaList).toListForward should be(scalaList)
  }

  it should "be convertible from and to a Scala list when empty" in {
    val scalaList = List()
    DoublyLinkedList.fromList(scalaList).toListForward should be(scalaList)
  }

  it should "be convertible to a reversed Scala list when populated" in {
    val scalaList = List(1, 2, 3, 4, 5, 6)
    DoublyLinkedList.fromList(scalaList).toListReversed should be(List(6, 5, 4, 3, 2, 1))
  }

  it should "be convertible to a reversed Scala list when empty" in {
    val scalaList = List()
    DoublyLinkedList.fromList(scalaList).toListReversed should be(scalaList)
  }

  it should "be able to be created from a list with a single item" in {
    DoublyLinkedList.fromList(List(1)) should be(
      DoublyLinkedList(
        Some(DoublyLinkedNode[Int](1, None, None)),
        Some(DoublyLinkedNode[Int](1, None, None))
      )
    )
  }

}
