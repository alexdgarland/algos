package crackingthecodinginterview.arraysandstrings

object Matrix {

  case class Indices(rowIndex: Int, columnIndex: Int)

  class ExtendedMatrix(matrix: Array[Array[Int]]) {
    def get(indices: Indices): Int = matrix(indices.rowIndex)(indices.columnIndex)
    def set(indices: Indices, value: Int): Unit = matrix(indices.rowIndex)(indices.columnIndex) = value
  }

  implicit def extendedMatrix(matrix: Array[Array[Int]]): ExtendedMatrix = new ExtendedMatrix(matrix)

  sealed trait RotationDirection
  case object Clockwise extends RotationDirection
  case object AntiClockwise extends RotationDirection

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
        inner(j) = direction match {
          case Clockwise => input(size - (j + 1))(i)
          case AntiClockwise => input(j)(size - (i + 1))
        }
      }
      outer(i) = inner
    }
    outer
  }

  /***
   * Rotate a matrix in place, hence saving space. Runs in O(n squared).
   *
   * @param matrix
   */
  def rotateMatrixInPlace(matrix: Array[Array[Int]], direction: RotationDirection = Clockwise): Unit = {
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
        val copySequence = direction match {
          case Clockwise => List(topPoint, leftPoint, bottomPoint, rightPoint)
          case AntiClockwise => List(topPoint, rightPoint, bottomPoint, leftPoint)
        }
        // This temp var only ever takes constant space of 1 value
        val temp = matrix.get(topPoint)
        // If we're being very precise, note that the four set calls here cancel out the 0.5 factors above
        // and mean that we get *exactly* n-squared (minus 1 where n is odd) calls
        (0 to 2).foreach { i => matrix.set(copySequence(i), matrix.get(copySequence(i + 1))) }
        matrix.set(copySequence.last, temp)
      }
    }
  }

  /***
   * If an element in an MxN matrix is 0, its entire row and column are set to 0.
   *
   * @param matrix
   */
  def zeroMatrix(matrix: Array[Array[Int]]): Unit = {
    // For an n * m matrix:
    // O(m) - initialise a single array the same length (number of columns) as each row
    val columnZeroes = Array.fill[Boolean](matrix(0).length)(false)
    // Loop n times
    matrix.foreach { row =>
      var rowHasZeroes = false
      // Loop m times (within outer loop)
      row.indices.foreach { columnIndex =>
        if(row(columnIndex) == 0) {
          // We can't zero out the whole row straight away
          // because we still need to check for any other columns we can mark as containing zeroes
          // We could optimise by checking if all columns already have zeroes...
          //...doing so naively could itself involve repeatedly passing through columnZeroes (O(m))
          // which would defeat the point, but could record and decrement the count of non-zero columns,
          // then if it is zero short-circuit logic
          rowHasZeroes = true
          columnZeroes(columnIndex) = true
        }
      }
      if(rowHasZeroes) {
        // Loop m times (within same outer loop)
        row.indices.foreach { columnIndex => row(columnIndex) = 0 }
      }
    }
    // Another O(n * m) loop to actually set things
    matrix.foreach { row =>
      // Could skip the inner loop for rows we have already zeroed out if we somehow kept track of that?
      // Tracking in an array would take extra space but given that we've already zeroed them out
      // we could just check the first element?
      row.indices.foreach { columnIndex =>
        if(columnZeroes(columnIndex)) { row(columnIndex) = 0 }
      }
    }
    // Might be able to optimise further by skipping columns/ rows we already know are zeroed out?
    // Would leave time complexity as O(n * m) but would shave some time off here and there.
  }

}
