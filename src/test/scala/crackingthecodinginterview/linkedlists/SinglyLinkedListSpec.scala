package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SinglyLinkedListSpec extends AnyFlatSpec with should.Matchers {

  "SinglyLinkedList" should "be convertible from and to a Scala list when populated" in {
    val scalaList = List(1, 2, 3, 4, 5, 6)
    SinglyLinkedList.fromList(scalaList).toList() should be(scalaList)
  }

  it should "be convertible from and to a Scala list when empty" in {
    val scalaList = List[Int]()
    SinglyLinkedList.fromList(scalaList).toList() should be(scalaList)
  }

  it should "allow appending a value when populated" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.append(4)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow appending a value when not populated" in {
    val linkedList = SinglyLinkedList.fromList[Int](List())
    linkedList.append(4)
    linkedList.toList() should be(List(4))
  }

  it should "allow inserting a value within list when populated" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 2)
    linkedList.toList() should be(List(1, 2, 4, 3))
  }

  it should "allow inserting a value to start of list when populated" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 0)
    linkedList.toList() should be(List(4, 1, 2, 3))
  }

  it should "allow prepending a value to list when populated" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.prepend(4)
    linkedList.toList() should be(List(4, 1, 2, 3))
  }

  it should "allow inserting a value to end of list when populated" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.insertAt(4, 3)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow inserting a value when not populated" in {
    val linkedList = SinglyLinkedList.fromList[Int](List())
    linkedList.insertAt(4, 0)
    linkedList.toList() should be(List(4))
  }

  it should "not allow insertion of a value at a negative index" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, -1)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of a populated list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 4)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of an empty list" in {
    val linkedList = SinglyLinkedList.fromList[Int](List())
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 1)
    }
  }

  it should "allow deletion from a populated list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList.deleteAt(1)
    linkedList.toList() should be(List(1, 3))
  }

  it should "not allow deletion from an empty list" in {
    val linkedList = SinglyLinkedList[Int]()
    assertThrows[IndexOutOfBoundsException] {
      linkedList.deleteAt(0)
    }
  }

  it should "not allow deletion at an index beyond the end of a populated list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    assertThrows[IndexOutOfBoundsException] {
      linkedList.deleteAt(3)
    }
  }

  it should "allow deletion based on a predicate where last item meets predicate" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i % 2 == 0)
    linkedList.toList() should be(List(1, 3))
  }

  it should "allow deletion based on a predicate where first item meets predicate" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i % 2 == 1)
    linkedList.toList() should be(List(2, 4))
  }

  it should "not alter list or fail when deletion predicate is not matched" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i > 10)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow emptying of list where all items meet predicate" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3, 4))
    linkedList.deleteWhere(i => i < 5)
    linkedList.toList() should be(List())
  }

  it should "allowing mapping a function over a populated list" in {
    SinglyLinkedList.fromList(List(1, 2, 3)).map(i => s"Number $i").toList() should be(List("Number 1", "Number 2", "Number 3"))
  }

  it should "allowing mapping a function over an empty list" in {
    SinglyLinkedList[Int]().map(i => s"Number $i").toList() should be(List())
  }

  it should "allow deduplication" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 1, 2, 3, 4, 4, 4, 2, 5, 5, 3, 6, 6, 7))
    linkedList.deduplicate()
    linkedList.toList() should be(List(1, 2, 3, 4, 5, 6, 7))
  }

  "deleteNode object method" should "be able to delete node from list without access to full list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    val midNode = linkedList.head.get.next.get
    SinglyLinkedList.deleteNode(midNode)
    linkedList.toList() should be(List(1, 3))
  }

  it should "be able to delete first node from list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    val headNode = linkedList.head.get
    SinglyLinkedList.deleteNode(headNode)
    linkedList.toList() should be(List(2, 3))
  }

  it should "throw an error when passed only node in list" in {
    val linkedList = SinglyLinkedList.fromList(List(1))
    val onlyNode = linkedList.head.get
    assertThrows[IllegalArgumentException] {
      SinglyLinkedList.deleteNode(onlyNode)
    }
  }

  it should "throw an error when passed final node in list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    val finalNode = linkedList.head.get.next.get.next.get
    assertThrows[IllegalArgumentException] {
      SinglyLinkedList.deleteNode(finalNode)
    }
  }

  "size" should "return zero for an empty list" in {
    SinglyLinkedList.fromList(List[Int]()).length should be(0)
  }

  it should "return correct size for populated list" in {
    SinglyLinkedList.fromList(List(1, 2, 3)).length should be(3)
  }

  "applying an index" should "be able to get appropriate node from list" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList(1).map(_.value) should be(Some(2))
  }

  it should "return None where index is out of range" in {
    val linkedList = SinglyLinkedList.fromList(List(1, 2, 3))
    linkedList(3) should be(None)
  }

  "kthFromLast" should "return expected node when available" in {
    SinglyLinkedList.fromList(List(1, 2, 3, 4, 5, 6)).kthFromLast(4)
      .map(_.value) should be(Some(2))
  }

  it should "return None where k is out of range" in {
    SinglyLinkedList.fromList(List(1, 2, 3, 4, 5, 6)).kthFromLast(6) shouldBe None
  }

  "partition" should "rearrange the list as expected" in {
    val originalValues = List(3, 5, 8, 5, 10, 2, 1)
    val linkedList = SinglyLinkedList.fromList(originalValues)
    linkedList.partition(5)
    val newValues = linkedList.toList()
    newValues.take(3).foreach(_ should be < 5)
    newValues.drop(3).foreach(_ should be >= 5)
    newValues.sorted should be(originalValues.sorted)
  }

  "sumLists function" should "be able to add two lists of ints as specified" in {
    val list617 = SinglyLinkedList.fromList(List(7, 1, 6))
    val list295 = SinglyLinkedList.fromList(List(5, 9, 2))
    val expectedList912 = SinglyLinkedList.fromList(List(2, 1, 9))
    SinglyLinkedList.sumLists(list617, list295) should be(expectedList912)
  }

}
