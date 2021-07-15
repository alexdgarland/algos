package crackingthecodinginterview.arraysandstrings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import IsUnique._

class IsUniqueSpec extends AnyFlatSpec with should.Matchers {

  "isUnique function" should "return true where no duplicate characters exist" in {
    isUnique("abcde") should be(true)
  }

  it should "return false where duplicate characters exist" in {
    isUnique("abcade") should be(false)
  }

  "isUniqueNoDataStructures function" should "return true where no duplicate characters exist" in {
    isUniqueNoDataStructures("abcde") should be(true)
  }

  it should "return false where duplicate characters exist" in {
    isUniqueNoDataStructures("abcade") should be(false)
  }

}
