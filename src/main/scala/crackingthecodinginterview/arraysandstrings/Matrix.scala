package crackingthecodinginterview.arraysandstrings

object Matrix {

  case class Indices(rowIndex: Int, columnIndex: Int)

  class ExtendedMatrix(matrix: Array[Array[Int]]) {
    def get(indices: Indices): Int = {
      matrix(indices.rowIndex)(indices.columnIndex)
    }
    def set(indices: Indices, value: Int): Unit = {
      matrix(indices.rowIndex)(indices.columnIndex) = value
    }
  }

  implicit def extendedMatrix(matrix: Array[Array[Int]]): ExtendedMatrix = new ExtendedMatrix(matrix)

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

  /***
   * Rotate a matrix in place, hence saving space. Runs in O(n squared).
   *
   * TODO - make it possible to rotate anti-clockwise as well
   *
   * @param matrix
   */
  def rotateMatrixInPlace(matrix: Array[Array[Int]]): Unit = {
    // Outer loop is O(n) (constant multiple of 0.5, ignoring the absolute middle point in n is odd)
    (0 until matrix.length / 2).foreach { layerOffset =>
      val lowerBound = layerOffset
      val upperBound = matrix.length - (layerOffset + 1)
      // Inner loop runs in O(n), with an average multiple of 0.5
      (lowerBound until upperBound).foreach { pointOffset =>
          val topPoint = Indices(lowerBound, lowerBound + pointOffset)
          val rightPoint = Indices(lowerBound + pointOffset, upperBound)
          val bottomPoint = Indices(upperBound, upperBound - pointOffset)
          val leftPoint = Indices(upperBound - pointOffset, lowerBound)
          // This temp var only ever takes constant space of 1 value
          val temp = matrix.get(topPoint)
          // If we're being very precise, note that the four set calls here cancel out the 0.5 factors above
          // and mean that we get *exactly* n-squared (minus 1 where n is odd) calls
          matrix.set(topPoint, matrix.get(leftPoint))
          matrix.set(leftPoint, matrix.get(bottomPoint))
          matrix.set(bottomPoint, matrix.get(rightPoint))
          matrix.set(rightPoint, temp)
      }
    }
  }

}
