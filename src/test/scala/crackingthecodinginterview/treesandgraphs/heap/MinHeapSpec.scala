package crackingthecodinginterview.treesandgraphs.heap

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MinHeapSpec extends AnyFlatSpec with should.Matchers {

  "visualise" should "display graphic version of full heap tree" in {
    val heap = new MinHeap[Int]()
    (1 to 7).foreach(heap.insert)
    val expected = """
                     |      1
                     |  2       3
                     |4   5   6   7
                     |""".stripMargin
    heap.visualise should be(expected)
  }

  it should "display graphic version of non-full heap tree" in {
    val heap = new MinHeap[Int]()
    (1 to 6).foreach(heap.insert)
    val expected = """
                     |      1
                     |  2       3
                     |4   5   6
                     |""".stripMargin
    heap.visualise should be(expected)
  }

  it should "display graphic version of heap tree with double-digit numbers" in {
    val heap = new MinHeap[Int]()
    (1 to 15).foreach(heap.insert)
    val expected =
      """
        |              1
        |      2               3
        |  4       5       6       7
        |8   9   10  11  12  13  14  15
        |""".stripMargin
    heap.visualise should be(expected)
  }

}