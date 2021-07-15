package crackingthecodinginterview.arraysandstrings

import crackingthecodinginterview.arraysandstrings.Matrix.AntiClockwise
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatrixSpec extends AnyFlatSpec with should.Matchers {

  // May need to change this if and when we start doing in-place transformations
  val input: Array[Array[Int]] = Array(
    Array(1, 2, 3, 4),
    Array(5, 6, 7, 8),
    Array(9, 10, 11, 12),
    Array(13, 14, 15, 16)
  )

  "rotateMatrix function" should "rotate a matrix by 90 degrees clockwise by default" in {
    Matrix.rotateMatrix(input) should be(
      Array(
        Array(13, 9, 5, 1),
        Array(14, 10, 6, 2),
        Array(15, 11, 7, 3),
        Array(16, 12, 8, 4)
      )
    )
  }

  "rotateMatrix function" should "rotate be able to rotate a matrix by 90 degrees anti-clockwise" in {
    Matrix.rotateMatrix(input, AntiClockwise) should be(
      Array(
        Array(4, 8, 12, 16),
        Array(3, 7, 11, 15),
        Array(2, 6, 10, 14),
        Array(1, 5, 9, 13)
      )
    )
  }

}
