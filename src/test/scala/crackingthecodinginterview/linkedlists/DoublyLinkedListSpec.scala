package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DoublyLinkedListSpec extends AnyFlatSpec with should.Matchers {

  /**
   * Compare the produced LinkedList to expected values both forward and backwards,
   * to make sure both sets of pointers (prev and next for each node, plus head and tail) are in place as expected.
   *
   * @param linkedList        Actual list.
   * @param expectedScalaList Expected values.
   * @tparam T Type of values in lists.
   */
  private def biDirectionalCompare[T](linkedList: DoublyLinkedList[T], expectedScalaList: List[T]): Unit = {
    linkedList.toList() should be(expectedScalaList)
    linkedList.toListReversed should be(expectedScalaList.reverse)
  }

  "DoublyLinkedList" should "be convertible to a Scala list when populated" in {
    DoublyLinkedList(1, 2, 3, 4, 5, 6).toList() should be(List(1, 2, 3, 4, 5, 6))
  }

  it should "be convertible to a Scala list when empty" in {
    DoublyLinkedList[Int]().toList() should be(List[Int]())
  }

  it should "be convertible to a reversed Scala list when populated" in {
    DoublyLinkedList(1, 2, 3, 4, 5, 6).toListReversed should be(List(6, 5, 4, 3, 2, 1))
  }

  it should "be convertible to a reversed Scala list when empty" in {
    DoublyLinkedList[Int]().toListReversed should be(List[Int]())
  }

  it should "be able to be created from a list with a single item" in {
    DoublyLinkedList(1) should be(
      DoublyLinkedList(
        Some(DoublyLinkedNode[Int](1, None, None)),
        Some(DoublyLinkedNode[Int](1, None, None))
      )
    )
  }

  it should "allow appending a value when populated" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.append(4)
    biDirectionalCompare(linkedList, List(1, 2, 3, 4))
  }

  it should "allow appending a value when not populated" in {
    val linkedList = DoublyLinkedList[Int]()
    linkedList.append(4)
    biDirectionalCompare(linkedList, List(4))
  }

  it should "allow prepending a value to list when populated" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.prepend(4)
    biDirectionalCompare(linkedList, List(4, 1, 2, 3))
  }

  it should "allow prepending a value to list when empty" in {
    val linkedList = DoublyLinkedList[Int]()
    linkedList.prepend(4)
    biDirectionalCompare(linkedList, List(4))
  }

  it should "allow inserting a value within list when populated" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 2)
    biDirectionalCompare(linkedList, List(1, 2, 4, 3))
  }

  it should "allow inserting a value to start of list when populated" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 0)
    biDirectionalCompare(linkedList, List(4, 1, 2, 3))
  }

  it should "allow inserting a value to end of list when populated" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.insertAt(4, 3)
    biDirectionalCompare(linkedList, List(1, 2, 3, 4))
  }

  it should "allow inserting a value when not populated" in {
    val linkedList = DoublyLinkedList[Int]()
    linkedList.insertAt(4, 0)
    biDirectionalCompare(linkedList, List(4))
  }

  it should "not allow insertion of a value at a negative index" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, -1)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of a populated list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 4)
    }
  }

  it should "not allow insertion of a value at an index beyond the end of an empty list" in {
    val linkedList = DoublyLinkedList[Int]()
    assertThrows[IndexOutOfBoundsException] {
      linkedList.insertAt(4, 1)
    }
  }

  it should "allow deletion from a populated list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.deleteAt(1)
    biDirectionalCompare(linkedList, List(1, 3))
  }

  it should "not allow deletion from an empty list" in {
    assertThrows[IndexOutOfBoundsException] {
      DoublyLinkedList[Int]().deleteAt(0)
    }
  }

  it should "not allow deletion at an index beyond the end of a populated list" in {
    assertThrows[IndexOutOfBoundsException] {
      DoublyLinkedList(1, 2, 3).deleteAt(3)
    }
  }

  it should "allow deletion based on a predicate where last item meets predicate" in {
    val linkedList = DoublyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i % 2 == 0)
    biDirectionalCompare(linkedList, List(1, 3))
  }

  it should "allow deletion based on a predicate where first item meets predicate" in {
    val linkedList = DoublyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i % 2 == 1)
    biDirectionalCompare(linkedList, List(2, 4))
  }

  it should "not alter list or fail when deletion predicate is not matched" in {
    val linkedList = DoublyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i > 10)
    biDirectionalCompare(linkedList, List(1, 2, 3, 4))
  }

  it should "allow emptying of list where all items meet predicate" in {
    val linkedList = DoublyLinkedList(1, 2, 3, 4)
    linkedList.deleteWhere(i => i < 5)
    biDirectionalCompare(linkedList, List())
  }

  it should "allowing mapping a function over a populated list" in {
    val mappedList = DoublyLinkedList(1, 2, 3).map(i => s"Number $i")
    biDirectionalCompare(mappedList, List("Number 1", "Number 2", "Number 3"))
  }

  it should "allowing mapping a function over an empty list" in {
    val mappedList = DoublyLinkedList[Int]().map(i => s"Number $i")
    biDirectionalCompare(mappedList, List())
  }

  it should "allow deduplication" in {
    val linkedList = DoublyLinkedList(1, 1, 2, 3, 4, 4, 4, 2, 5, 5, 3, 6, 6, 7)
    linkedList.deduplicate()
    biDirectionalCompare(linkedList, List(1, 2, 3, 4, 5, 6, 7))
  }

  "deleteNode object method" should "be able to delete node from list without access to full list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    val midNode = linkedList.head.get.next.get
    DoublyLinkedList.deleteNode(midNode)
    biDirectionalCompare(linkedList, List(1, 3))
  }

  it should "be able to delete first node from list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    val headNode = linkedList.head.get
    DoublyLinkedList.deleteNode(headNode)
    biDirectionalCompare(linkedList, List(2, 3))
  }

  it should "be able to delete final node from list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    val finalNode = linkedList.head.get.next.get.next.get
    DoublyLinkedList.deleteNode(finalNode)
    biDirectionalCompare(linkedList, List(1, 2))
  }

  it should "throw an error when passed only node in list" in {
    val linkedList = DoublyLinkedList(1)
    val onlyNode = linkedList.head.get
    assertThrows[IllegalArgumentException] {
      DoublyLinkedList.deleteNode(onlyNode)
    }
  }

  "size" should "return zero for an empty list" in {
    DoublyLinkedList[Int]().length should be(0)
  }

  it should "return correct size for populated list" in {
    DoublyLinkedList(1, 2, 3).length should be(3)
  }

  "applying an index" should "be able to get appropriate node from list" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList(1).map(_.value) should be(Some(2))
  }

  it should "return None where index is out of range" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList(3) shouldBe None
  }

  "kthFromLast" should "return expected node when available" in {
    DoublyLinkedList(1, 2, 3, 4, 5, 6).kthFromLast(4)
      .map(_.value) should be(Some(2))
  }

  it should "return None where k is out of range" in {
    DoublyLinkedList(1, 2, 3, 4, 5, 6).kthFromLast(6) shouldBe None
  }

  "partition" should "rearrange the list as expected" in {
    val originalValues = List(3, 5, 8, 5, 10, 2, 1)
    val linkedList = DoublyLinkedList(originalValues: _*)

    linkedList.partition(5)

    val newValuesFromHead = linkedList.toList()
    newValuesFromHead.take(3).foreach(_ should be < 5)
    newValuesFromHead.drop(3).foreach(_ should be >= 5)
    newValuesFromHead.sorted should be(originalValues.sorted)

    val newValuesFromTail = linkedList.toListReversed
    newValuesFromTail.take(4).foreach(_ should be >= 5)
    newValuesFromTail.drop(4).foreach(_ should be < 5)
    newValuesFromTail.sorted should be(originalValues.sorted)
  }

  "sumLists function" should "be able to add two lists of ints as specified" in {
    val list1 = DoublyLinkedList(7, 1, 6)
    val list2 = DoublyLinkedList(5, 9, 2)
    val summed = DoublyLinkedList.sumLists(list1, list2)
    biDirectionalCompare(summed, List(2, 1, 9))
  }

  it should "be able to add two lists of ints of different lengths" in {
    val list1 = DoublyLinkedList(7, 1, 6, 1)
    val list2 = DoublyLinkedList(5, 9, 2)
    val summed = DoublyLinkedList.sumLists(list1, list2)
    biDirectionalCompare(summed, List(2, 1, 9, 1))
  }

  it should "be able to add two lists when the total length is longer due to a carry" in {
    val list1 = DoublyLinkedList(7, 9, 6)
    val list2 = DoublyLinkedList(5, 9, 3)
    val summed = DoublyLinkedList.sumLists(list1, list2)
    biDirectionalCompare(summed, List(2, 9, 0, 1))
  }

  "reverse" should "perform an in-place reversal of list nodes" in {
    val linkedList = DoublyLinkedList(1, 2, 3)
    linkedList.reverse()
    biDirectionalCompare(linkedList, List(3, 2, 1))
  }

  it should "be able to handle an empty list" in {
    val linkedList = DoublyLinkedList[Int]()
    linkedList.reverse()
    biDirectionalCompare(linkedList, List())
  }

  "isPalindrome" should "indicate that an empty list is a palindrome" in {
    DoublyLinkedList[String]().isPalindrome shouldBe true
  }

  it should "identify an even-length palindrome" in {
    DoublyLinkedList("a", "b", "c", "b", "b", "c", "b", "a").isPalindrome shouldBe true
  }

  it should "identify an odd-length palindrome" in {
    DoublyLinkedList("a", "b", "c", "b", "c", "b", "a").isPalindrome shouldBe true
  }

  it should "identify an even-length non-palindrome" in {
    DoublyLinkedList("a", "b", "c", "b", "d", "c", "b", "a").isPalindrome shouldBe false
  }

  it should "identify an odd-length non-palindrome" in {
    DoublyLinkedList("a", "b", "c", "b", "d", "b", "a").isPalindrome shouldBe false
  }

  "foldLeft" should "accumulate as expected" in {
    DoublyLinkedList(1, 2, 3, 4)
      .foldLeft("0")((stringAccumulator, intValue) => s"$stringAccumulator$intValue") should be("01234")
  }

  "foldLeftNodes" should "accumulate as expected" in {
    DoublyLinkedList(1, 2, 3, 4)
      .foldLeftNodes("0")((stringAccumulator, node) => s"$stringAccumulator${node.value}") should be("01234")
  }

  "find" should "get Some of first matching element where present" in {
    DoublyLinkedList(1, 2, 3, 4).find(_ > 2) should be(Some(3))
  }

  it should "return None where no matching element present" in {
    DoublyLinkedList(1, 2, 3, 4).find(_ > 5) should be(None)
  }

  it should "return None for an empty linked list" in {
    DoublyLinkedList[Int]().find(_ > 2) should be(None)
  }

}
