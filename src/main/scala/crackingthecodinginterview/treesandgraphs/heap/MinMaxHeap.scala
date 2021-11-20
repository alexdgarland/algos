package crackingthecodinginterview.treesandgraphs.heap

import scala.collection.mutable.ArrayBuffer

abstract class MinMaxHeap[T](private val ordering: Ordering[T]) {

  private val array = ArrayBuffer[T]()

  def toList: List[T] = array.toList

  def height: Int = (math.log(array.size) / math.log(2.0)).ceil.toInt

  def insert(value: T): Unit = {
    array.append(value)
    bubbleUp()
  }

  def peek(): Option[T] = array.headOption

  def pop(): Option[T] = {
    val minToReturn = peek()
    if(minToReturn.isDefined) {
      bubbleDown()
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

  private def swapAt(i1: Int, i2: Int): Unit = {
    val tmp = array(i1)
    array(i1) = array(i2)
    array(i2) = tmp
  }

  private implicit class BubbleComparisonValue(value: T) {
    import ordering.mkOrderingOps
    // By injecting reverse ordering (for MaxHeap), the "<" here can effectively be flipped to act as ">" instead.
    // The private wrapper class provides clarity of intent where the operation is used.
    def shouldBeAbove(other: T): Boolean = value < other
  }

  private def bubbleUp(): Unit = {
    var currentIndex = array.size - 1
    var reachedCorrectLevel = false
    while(currentIndex != 0 && !reachedCorrectLevel) {
      val parentIndex = (currentIndex / 2.0).ceil.toInt - 1
      if(array(currentIndex) shouldBeAbove array(parentIndex)) {
        swapAt(parentIndex, currentIndex)
      }
      else {
        reachedCorrectLevel = true
      }
      currentIndex = parentIndex
    }
  }

  private def bubbleDown(): Unit = {
    array(0) = array.remove(array.size - 1)
    var currentIndex = 0
    var reachedCorrectLevel = false
    while (currentIndex < (array.size - 1) && !reachedCorrectLevel) {
      val eligibleChildIndices = (1 to 2)
        .map((2 * currentIndex) + _)
        .filter(i => i < array.size && (array(i) shouldBeAbove array(currentIndex)))
      eligibleChildIndices
        .sortBy(array(_))(ordering)
        .headOption match {
        case None =>
          reachedCorrectLevel = true
        case Some(childIndexToSwap) =>
          swapAt(childIndexToSwap, currentIndex)
          currentIndex = childIndexToSwap
      }
    }
  }

}

class MinHeap[T](implicit val ordering: Ordering[T]) extends MinMaxHeap[T](ordering)

class MaxHeap[T](implicit val ordering: Ordering[T]) extends MinMaxHeap[T](ordering.reverse)
