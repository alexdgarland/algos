package crackingthecodinginterview.linkedlists

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

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

}
