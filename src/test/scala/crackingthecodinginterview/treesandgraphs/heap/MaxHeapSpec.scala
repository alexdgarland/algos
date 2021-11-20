package crackingthecodinginterview.treesandgraphs.heap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MaxHeapSpec extends AnyFlatSpec with should.Matchers {

  "visualise" should "display graphic version of full heap tree" in {
    val heap = new MaxHeap[Int]()
    (7 to 1 by -1).foreach(heap.insert)
    val expected = """
                     |      7
                     |  6       5
                     |4   3   2   1
                     |""".stripMargin
    heap.visualise should be(expected)
  }

  it should "display graphic version of non-full heap tree" in {
    val heap = new MaxHeap[Int]()
    (6 to 1 by -1).foreach(heap.insert)
    val expected = """
                     |      6
                     |  5       4
                     |3   2   1
                     |""".stripMargin
    heap.visualise should be(expected)
  }

  it should "display graphic version of heap tree with double-digit numbers" in {
    val heap = new MaxHeap[Int]()
    (15 to 1 by -1).foreach(heap.insert)
    val expected =
      """
        |              15
        |      14              13
        |  12      11      10      9
        |8   7   6   5   4   3   2   1
        |""".stripMargin
    heap.visualise should be(expected)
  }

  "peek" should "indicate maximum where elements were added in descending order" in {
    val heap = new MaxHeap[Int]()
    (6 to 1 by -1).foreach(heap.insert)
    heap.peek() should be(Some(6))
  }

  it should "indicate maximum where elements were added in arbitrary order" in {
    val heap = new MaxHeap[Int]()
    List(3, 6, 4, 1, 5, 2).foreach(heap.insert)
    heap.peek() should be(Some(6))
  }

  it should "return None when heap is empty" in {
    new MaxHeap[Int]().peek() should be(None)
  }

  "pop" should "remove maximum element while maintaining heap property" in {
    val heap = new MaxHeap[Int]()
    List(3, 6, 4, 1, 5, 2).foreach(heap.insert)
    heap.pop() should be(Some(6))
    heap.peek() should be(Some(5))
  }

  it should "return None when heap is empty" in {
    new MaxHeap[Int]().pop() should be(None)
  }

}
