package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import org.scalatest.Assertions.assertThrows

class LinkedListBaseSpec extends AnyFlatSpec with should.Matchers {

  "LinkedList" should "be convertible from and to a Scala list when populated" in {
    val scalaList = List(1, 2, 3, 4, 5, 6)
    LinkedList.fromList(scalaList).toList should be(scalaList)
  }

  it should "be convertible from and to a Scala list when empty" in {
    val scalaList = List()
    LinkedList.fromList(scalaList).toList should be(scalaList)
  }

  it should "be possible to append a value when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.append(4)
    linkedList.toList should be(List(1, 2, 3, 4))
  }

  it should "be possible to append a value when not populated" in {
    val linkedList = LinkedList.fromList(List())
    linkedList.append(4)
    linkedList.toList should be(List(4))
  }

  it should "be possible to insert a value within list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insert(4, 2)
    linkedList.toList should be(List(1, 2, 4, 3))
  }

  it should "be possible to insert a value to start of list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insert(4, 0)
    linkedList.toList should be(List(4, 1, 2, 3))
  }

  it should "be possible to insert a value to end of list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insert(4, 3)
    linkedList.toList should be(List(1, 2, 3, 4))
  }

  it should "be possible to insert a value when not populated" in {
    val linkedList = LinkedList.fromList(List())
    linkedList.insert(4, 0)
    linkedList.toList should be(List(4))
  }

  it should "not be possible to insert a value at a negative index" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insert(4, -1)
    }
  }

  it should "not be possible to insert a value at an index beyond the end of a populated list" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insert(4, 4)
    }
  }

  it should "not be possible to insert a value at an index beyond the end of an empty list" in {
    val linkedList = LinkedList.fromList(List())
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insert(4, 1)
    }
  }

}