package crackingthecodinginterview.treesandgraphs.heap

import scala.collection.mutable.ArrayBuffer

class MinHeap[T](implicit ordering: Ordering[T]) {

  private val array = ArrayBuffer[T]()

  def toList: List[T] = array.toList

  def height: Int = (math.log(array.size) / math.log(2.0)).ceil.toInt

  def insert(value: T): Unit = {
    array.append(value)
    // TODO - maintain heap property
  }

  def popMin(): T = {
    val min = array.remove(0)
    // TODO - remove, while maintaining heap property
    min
  }

  def peekMin(): T = array.head

  /**
   * Visualise the heap as a tree.
   *
   * Very basic and currently assumes single-digit integers (i.e. one character of space needed) only.
   *
   * @return
   */
  def visualise: String = {
    (0 until height)
      .map { level =>
        val spaceBetween = " ".repeat(math.pow(2, height + 1 - level).toInt - 1)
        val edgeSpace = " ".repeat(math.pow(2, height - level).toInt - 2)
        val nodeIndicesAtLevel = (
          math.pow(2, level).toInt - 1
            until
            math.min(math.pow(2, level + 1).toInt - 1 , array.size)
          )
      nodeIndicesAtLevel.map{array}.mkString(edgeSpace, spaceBetween, "")
    }.mkString("\n", "\n", "\n")
  }

}
