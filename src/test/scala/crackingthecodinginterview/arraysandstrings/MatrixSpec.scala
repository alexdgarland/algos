package crackingthecodinginterview.arraysandstrings

import crackingthecodinginterview.arraysandstrings.Matrix.AntiClockwise
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MatrixSpec extends AnyFlatSpec with should.Matchers {

  private def getInputEvenSize = Array(
    Array(1, 2, 3, 4),
    Array(5, 6, 7, 8),
    Array(9, 10, 11, 12),
    Array(13, 14, 15, 16)
  )

  private val expectedClockwiseEvenSize = Array(
    Array(13, 9, 5, 1),
    Array(14, 10, 6, 2),
    Array(15, 11, 7, 3),
    Array(16, 12, 8, 4)
  )

  private val expectedAntiClockwiseEvenSize = Array(
    Array(4, 8, 12, 16),
    Array(3, 7, 11, 15),
    Array(2, 6, 10, 14),
    Array(1, 5, 9, 13)
  )

  private def getInputOddSize = Array(
    Array(1, 2, 3, 4, 5),
    Array(6, 7, 8, 9, 10),
    Array(11, 12, 13, 14, 15),
    Array(16, 17, 18, 19, 20),
    Array(21, 22, 23, 24, 25)
  )

  private val expectedClockwiseOddSize = Array(
    Array(21, 16, 11, 6, 1),
    Array(22, 17, 12, 7, 2),
    Array(23, 18, 13, 8, 3),
    Array(24, 19, 14, 9, 4),
    Array(25, 20, 15, 10, 5)
  )

  private val expectedAntiClockwiseOddSize = Array(
    Array(5, 10, 15, 20, 25),
    Array(4, 9, 14, 19, 24),
    Array(3, 8, 13, 18, 23),
    Array(2, 7, 12, 17, 22),
    Array(1, 6, 11, 16, 21)
  )

  "rotateMatrix function" should "produce a copy of a even-sized matrix rotated by 90 degrees clockwise by default" in {
    Matrix.rotateMatrix(getInputEvenSize) should be(expectedClockwiseEvenSize)
  }

  it should "be able to produce a copy of an even-sized matrix rotated by 90 degrees anti-clockwise" in {
    Matrix.rotateMatrix(getInputEvenSize, AntiClockwise) should be(expectedAntiClockwiseEvenSize)
  }

  it should "produce a copy of a odd-sized matrix rotated by 90 degrees clockwise by default" in {
    Matrix.rotateMatrix(getInputOddSize) should be(expectedClockwiseOddSize)
  }

  it should "be able to produce a copy of an odd-sized matrix rotated by 90 degrees anti-clockwise" in {
    Matrix.rotateMatrix(getInputOddSize, AntiClockwise) should be(expectedAntiClockwiseOddSize)
  }

  "rotateMatrixInPlace function" should "rotate an even-sized matrix by 90 degrees clockwise by default" in {
    val matrix = getInputEvenSize
    Matrix.rotateMatrixInPlace(matrix)
    matrix should be(expectedClockwiseEvenSize)
  }

  it should "rotate an odd-sized matrix by 90 degrees clockwise by default" in {
    val matrix = getInputOddSize
    Matrix.rotateMatrixInPlace(matrix)
    matrix should be(expectedClockwiseOddSize)
  }

}
