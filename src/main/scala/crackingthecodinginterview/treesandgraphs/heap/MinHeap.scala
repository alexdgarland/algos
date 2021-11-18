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
   * Very basic and currently assumes (in terms of producing a readable visualisatio)
   * that no more than double-digit integers (i.e. 1-2 characters of space needed) are used as the node type.
   *
   * @return
   */
  def visualise: String = {
    val maxCharWidth = array.map{_.toString.length}.max
    (0 until height)
      .map { level =>
        val spaceBetween = " ".repeat(math.pow(2, height + 1 - level).toInt - maxCharWidth)
        val edgeSpace = " ".repeat(math.pow(2, height - level).toInt - 2)
        val firstIndexAtLevel = math.pow(2, level).toInt - 1
        val lastIndexAtLevel = math.min(math.pow(2, level + 1).toInt - 1 , array.size) - 1
        (firstIndexAtLevel to lastIndexAtLevel)
          .map{array(_).toString.padTo(maxCharWidth, ' ') }
          .mkString(edgeSpace, spaceBetween, "")
          .stripTrailing()
    }.mkString("\n", "\n", "\n")
  }

}
