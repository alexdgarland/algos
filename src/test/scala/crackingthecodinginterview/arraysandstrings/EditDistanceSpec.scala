package crackingthecodinginterview.arraysandstrings

import EditDistance._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class EditDistanceSpec extends AnyFlatSpec with should.Matchers {



  "isTransformableWithinSingleEdit function" should "return true if strings are already the same" in {
    isTransformableWithinSingleEdit("pale", "pale") should be(true)
  }

  it should "return true if removing a single character in middle will make strings match" in {
    isTransformableWithinSingleEdit("pale", "ple") should be(true)
  }

  it should "return true if removing a single at end character will make strings match" in {
    isTransformableWithinSingleEdit("pales", "pale") should be(true)
  }

  it should "return true if adding a single character will make strings match" in {
    isTransformableWithinSingleEdit("ple", "pale") should be(true)
  }

  it should "return true if changing a single character will make strings match" in {
    isTransformableWithinSingleEdit("pale", "bale") should be(true)
  }

  it should "return false if strings cannot be made to match in one edit" in {
    isTransformableWithinSingleEdit("pale", "bake") should be(false)
  }

  it should "return false where character to remove is not at the end of string" in {
    isTransformableWithinSingleEdit("broke", "bake") should be(false)
  }

}
