package crackingthecodinginterview.arraysandstrings

object Matrix {

  case class Indices(rowIndex: Int, ColumnIndex: Int)

  sealed trait RotationDirection {
    def getIndices(outer: Int, inner: Int, matrixSize: Int): Indices
  }

  case object Clockwise extends RotationDirection {
    override def getIndices(outer: Int, inner: Int, matrixSize: Int): Indices = Indices(matrixSize - (inner + 1), outer)
  }

  case object AntiClockwise extends RotationDirection {
    override def getIndices(outer: Int, inner: Int, matrixSize: Int): Indices = Indices(inner, matrixSize - (outer + 1))
  }

  /***
   * Rotate a square (n x n) matrix of 32-bit ints
   * (which are large enough to represent 4-byte pixels,
   * and certainly should be interchangeable in terms of algorithm time complexity).
   *
   * By default rotates 90 degrees clockwise, but can also handle anti-clockwise rotation.
   *
   * Best conceivable runtime is O(n squared) as there are that many elements in the matrix
   * and each needs to be touched at least once - should simply try not to exceed that.
   *
   * Optimising for space by doing an in-place rotation of a mutable matrix is potentially to follow.
   *
   * @param input - the matrix to be rotated
   * @param direction - the direction in which to rotate the matrix (defaults to Clockwise)
   * @return
   */
  def rotateMatrix(input: Array[Array[Int]], direction: RotationDirection = Clockwise): Array[Array[Int]] = {
    val size = input.length
    val outer = new Array[Array[Int]](size)
    input.indices.foreach { i =>
      val inner = new Array[Int](size)
      input.indices.foreach { j =>
        direction.getIndices(i, j, size) match {
          case Indices(row, column) => inner(j) = input(row)(column)
        }
      }
      outer(i) = inner
    }
    outer
  }

}
