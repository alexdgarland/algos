package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SinglyLinkedListSpec extends AnyFlatSpec with should.Matchers {

  "SinglyLinkedList" should "be convertible to a Scala list when populated" in {
    SinglyLinkedList(1, 2, 3, 4, 5, 6).toList() should be(List(1, 2, 3, 4, 5, 6))
  }

  it should "be convertible from and to a Scala list when empty" in {
    SinglyLinkedList[Int]().toList() should be(List[Int]())
  }

  it should "allow appending a value when populated" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.append(4)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow appending a value when not populated" in {
    val linkedList = SinglyLinkedList[Int]()
    linkedList.append(4)
    linkedList.toList() should be(List(4))
  }

  it should "allow inserting a value within list when populated" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 2)
    linkedList.toList() should be(List(1, 2, 4, 3))
  }

  it should "allow inserting a value to start of list when populated" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 0)
    linkedList.toList() should be(List(4, 1, 2, 3))
  }

  it should "allow prepending a value to list when populated" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.prepend(4)
    linkedList.toList() should be(List(4, 1, 2, 3))
  }

  it should "allow inserting a value to end of list when populated" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 3)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow inserting a value when not populated" in {
    val linkedList = SinglyLinkedList[Int]()
    linkedList.insertAt(4, 0)
    linkedList.toList() should be(List(4))
  }

  it should "not allow insertion of a value at a negative index" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, -1)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of a populated list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 4)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of an empty list" in {
    val linkedList = SinglyLinkedList[Int]()
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 1)
    }
  }

  it should "allow deletion from a populated list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
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
    val linkedList = SinglyLinkedList(1, 2, 3)
    assertThrows[IndexOutOfBoundsException] {
      linkedList.deleteAt(3)
    }
  }

  it should "allow deletion based on a predicate where last item meets predicate" in {
    val linkedList = SinglyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i % 2 == 0)
    linkedList.toList() should be(List(1, 3))
  }

  it should "allow deletion based on a predicate where first item meets predicate" in {
    val linkedList = SinglyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i % 2 == 1)
    linkedList.toList() should be(List(2, 4))
  }

  it should "not alter list or fail when deletion predicate is not matched" in {
    val linkedList = SinglyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i > 10)
    linkedList.toList() should be(List(1, 2, 3, 4))
  }

  it should "allow emptying of list where all items meet predicate" in {
    val linkedList = SinglyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i < 5)
    linkedList.toList() should be(List())
  }

  it should "allowing mapping a function over a populated list" in {
    SinglyLinkedList(1, 2, 3).map(i => s"Number $i").toList() should be(List("Number 1", "Number 2", "Number 3"))
  }

  it should "allowing mapping a function over an empty list" in {
    SinglyLinkedList[Int]().map(i => s"Number $i").toList() should be(List())
  }

  it should "allow deduplication" in {
    val linkedList = SinglyLinkedList(1, 1, 2, 3, 4, 4, 4, 2, 5, 5, 3, 6, 6, 7)
    linkedList.deduplicate()
    linkedList.toList() should be(List(1, 2, 3, 4, 5, 6, 7))
  }

  "deleteNode object method" should "be able to delete node from list without access to full list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    val midNode = linkedList.head.get.next.get
    SinglyLinkedList.deleteNode(midNode)
    linkedList.toList() should be(List(1, 3))
  }

  it should "be able to delete first node from list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    val headNode = linkedList.head.get
    SinglyLinkedList.deleteNode(headNode)
    linkedList.toList() should be(List(2, 3))
  }

  it should "throw an error when passed only node in list" in {
    val linkedList = SinglyLinkedList(1)
    val onlyNode = linkedList.head.get
    assertThrows[IllegalArgumentException] {
      SinglyLinkedList.deleteNode(onlyNode)
    }
  }

  it should "throw an error when passed final node in list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    val finalNode = linkedList.head.get.next.get.next.get
    assertThrows[IllegalArgumentException] {
      SinglyLinkedList.deleteNode(finalNode)
    }
  }

  "size" should "return zero for an empty list" in {
    SinglyLinkedList[Int]().length should be(0)
  }

  it should "return correct size for populated list" in {
    SinglyLinkedList(1, 2, 3).length should be(3)
  }

  "applying an index" should "be able to get appropriate node from list" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList(1).map(_.value) should be(Some(2))
  }

  it should "return None where index is out of range" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList(3) should be(None)
  }

  "kthFromLast" should "return expected node when available" in {
    SinglyLinkedList(1, 2, 3, 4, 5, 6).kthFromLast(4)
      .map(_.value) should be(Some(2))
  }

  it should "return None where k is out of range" in {
    SinglyLinkedList(1, 2, 3, 4, 5, 6).kthFromLast(6) shouldBe None
  }

  "partition" should "rearrange the list as expected" in {
    val originalValues = List(3, 5, 8, 5, 10, 2, 1)
    val linkedList = SinglyLinkedList(originalValues: _*)
    linkedList.partition(5)
    val newValues = linkedList.toList()
    newValues.take(3).foreach(_ should be < 5)
    newValues.drop(3).foreach(_ should be >= 5)
    newValues.sorted should be(originalValues.sorted)
  }

  "sumLists function" should "be able to add two lists of ints as specified" in {
    val list1 = SinglyLinkedList(7, 1, 6)
    val list2 = SinglyLinkedList(5, 9, 2)
    SinglyLinkedList.sumLists(list1, list2).toList() should be(List(2, 1, 9))
  }

  it should "be able to add two lists of ints of different lengths" in {
    val list1 = SinglyLinkedList(7, 1, 6, 1)
    val list2 = SinglyLinkedList(5, 9, 2)
    SinglyLinkedList.sumLists(list1, list2).toList() should be(List(2, 1, 9, 1))
  }

  it should "be able to add two lists when the total length is longer due to a carry" in {
    val list1 = SinglyLinkedList(7, 9, 6)
    val list2 = SinglyLinkedList(5, 9, 3)
    SinglyLinkedList.sumLists(list1, list2).toList() should be(List(2, 9, 0, 1))
  }

  "reverse" should "perform an in-place reversal of list nodes" in {
    val linkedList = SinglyLinkedList(1, 2, 3)
    linkedList.reverse()
    linkedList.toList() should be(List(3, 2, 1))
  }

  it should "be able to handle an empty list" in {
    val linkedList = SinglyLinkedList[Int]()
    linkedList.reverse()
    linkedList.toList() should be(List())
  }

  "isPalindrome" should "indicate that an empty list is a palindrome" in {
    SinglyLinkedList[String]().isPalindrome shouldBe true
  }

  it should "identify an even-length palindrome" in {
    SinglyLinkedList("a", "b", "c", "b", "b", "c", "b", "a").isPalindrome shouldBe true
  }

  it should "identify an odd-length palindrome" in {
    SinglyLinkedList("a", "b", "c", "b", "c", "b", "a").isPalindrome shouldBe true
  }

  it should "identify an even-length non-palindrome" in {
    SinglyLinkedList("a", "b", "c", "b", "d", "c", "b", "a").isPalindrome shouldBe false
  }

  it should "identify an odd-length non-palindrome" in {
    SinglyLinkedList("a", "b", "c", "b", "d", "b", "a").isPalindrome shouldBe false
  }

  "intersectingNode" should "return None when lists do not intersect" in {
    val list1 = SinglyLinkedList(1, 2, 3)
    val list2 = SinglyLinkedList(1, 2, 3)
    list1.intersectingNode(list2) shouldBe None
  }

  it should "return Some of correct node when lists do intersect" in {
    val list1 = SinglyLinkedList(1, 2, 3, 4, 5)
    val expectedIntersectingNode = list1.head.get.next.get
    val list2 = SinglyLinkedList(2)
    list2.head.get.next = Some(expectedIntersectingNode)
    list2.prepend(2)
    list1.intersectingNode(list2).get should be theSameInstanceAs expectedIntersectingNode
    list2.intersectingNode(list1).get should be theSameInstanceAs expectedIntersectingNode
  }

  "foldLeft" should "accumulate as expected" in {
    SinglyLinkedList(1, 2, 3, 4)
      .foldLeft("0")((stringAccumulator, intValue) => s"$stringAccumulator$intValue") should be("01234")
  }

  "foldLeftNodes" should "accumulate as expected" in {
    SinglyLinkedList(1, 2, 3, 4)
      .foldLeftNodes("0")((stringAccumulator, node) => s"$stringAccumulator${node.value}") should be("01234")
  }

  "find" should "get Some of first matching element where present" in {
    SinglyLinkedList(1, 2, 3, 4).find(_ > 2) should be(Some(3))
  }

  it should "return None where no matching element present" in {
    SinglyLinkedList(1, 2, 3, 4).find(_ > 5) should be(None)
  }

  it should "return None for an empty linked list" in {
    SinglyLinkedList[Int]().find(_ > 2) should be(None)
  }

  "getLoopStart" should "return Some of node at start of loop" in {
    val list = SinglyLinkedList(1, 2, 3, 4)
    /*
    Transform list to:

    1 -> 2 -> 3 -> 4
         ^         |
         ---cycle---

     */
    list.findNode(_.value == 4).foreach(_.next = list.findNode(_.value == 2))
    list.getLoopStart.get.value should be(2)
  }

  it should "return None where there is no loop" in {
    SinglyLinkedList(1, 2, 3, 4).getLoopStart should be(None)
  }

}
