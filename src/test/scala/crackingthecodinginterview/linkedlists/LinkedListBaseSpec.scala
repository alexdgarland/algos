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

  it should "allow appending a value when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.append(4)
    linkedList.toList should be(List(1, 2, 3, 4))
  }

  it should "allow appending a value when not populated" in {
    val linkedList = LinkedList.fromList[Int](List())
    linkedList.append(4)
    linkedList.toList should be(List(4))
  }

  it should "allow inserting a value within list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 2)
    linkedList.toList should be(List(1, 2, 4, 3))
  }

  it should "allow inserting a value to start of list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 0)
    linkedList.toList should be(List(4, 1, 2, 3))
  }

  it should "allow inserting a value to end of list when populated" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 3)
    linkedList.toList should be(List(1, 2, 3, 4))
  }

  it should "allow inserting a value when not populated" in {
    val linkedList = LinkedList.fromList[Int](List())
    linkedList.insertAt(4, 0)
    linkedList.toList should be(List(4))
  }

  it should "not allow insertion of a value at a negative index" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, -1)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of a populated list" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 4)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of an empty list" in {
    val linkedList = LinkedList.fromList[Int](List())
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 1)
    }
  }

  it should "allow deletion from a populated list" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    linkedList.deleteAt(1)
    linkedList.toList should be(List(1, 3))
  }

  it should "not allow deletion from an empty list" in {
    val linkedList = LinkedList()
    assertThrows[IndexOutOfBoundsException] {
      linkedList.deleteAt(0)
    }
  }

  it should "not allow deletion at an index beyond the end of a populated list" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.deleteAt(3)
    }
  }

  it should "allow deletion based on a predicate where last item meets predicate" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i % 2 == 0)
    linkedList.toList should be(List(1, 3))
  }

  it should "allow deletion based on a predicate where first item meets predicate" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i % 2 == 1)
    linkedList.toList should be(List(2, 4))
  }

  it should "not alter list or fail when deletion predicate is not matched" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i > 10)
    linkedList.toList should be(List(1, 2, 3, 4))
  }

  it should "allow emptying of list where all items meet predicate" in {
    val linkedList = LinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i < 5)
    linkedList.toList should be(List())
  }

  it should "allowing mapping a function over a populated list" in {
    LinkedList.fromList(List(1, 2, 3)).map(i => s"Number $i").toList should be(List("Number 1", "Number 2", "Number 3"))
  }

  it should "allowing mapping a function over an empty list" in {
    LinkedList[Int]().map(i => s"Number $i").toList should be(List())
  }

}
