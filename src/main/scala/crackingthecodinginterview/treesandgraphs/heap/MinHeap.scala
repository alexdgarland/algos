package crackingthecodinginterview.treesandgraphs.heap

import scala.collection.mutable.ArrayBuffer

class MinHeap[T](implicit ordering: Ordering[T]) {
// TODO - any general cleanup?
// TODO - should peekMin/ popMin return Option[T] so can return None for empty heap?

  import ordering.mkOrderingOps

  private val array = ArrayBuffer[T]()

  def toList: List[T] = array.toList

  def height: Int = (math.log(array.size) / math.log(2.0)).ceil.toInt

  private def swapAt(i1: Int, i2: Int): Unit = {
    val tmp = array(i1)
    array(i1) = array(i2)
    array(i2) = tmp
  }

  def insert(value: T): Unit = {
    array.append(value)
    var currentIndex = array.size - 1
    var reachedCorrectLevel = false
    while(currentIndex != 0 && !reachedCorrectLevel) {
      val parentIndex = (currentIndex / 2.0).ceil.toInt - 1
      if(array(parentIndex) > array(currentIndex)) {
        swapAt(parentIndex, currentIndex)
      }
      else {
        reachedCorrectLevel = true
      }
      currentIndex = parentIndex
    }
  }

  def peekMin(): T = array.head

  def popMin(): T = {
    val minToReturn = peekMin()
    val swapped = array.remove(array.size - 1)
    array(0) = swapped
    var currentIndex = 0
    var reachedCorrectLevel = false
    while (currentIndex < (array.size - 1) && !reachedCorrectLevel) {
      val leftIndex = (2 * currentIndex) + 1
      val rightIndex = (2 * currentIndex) + 2
      val eligibleChildIndexes = List(leftIndex, rightIndex)
        .filter(i => i < array.size)
        .filter(i => array(i) < array(currentIndex))
      if (eligibleChildIndexes.isEmpty) {
        reachedCorrectLevel = true
      }
      else {
        val childIndexToSwap = eligibleChildIndexes.minBy{ array(_) }
        swapAt(childIndexToSwap, currentIndex)
        currentIndex = childIndexToSwap
      }
    }
    minToReturn
  }

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
