package hrank

import hrank.HRankUtil.{isPermutation, isPermutationOfPalindrome, isPermutationOfPalindromeConstantFactorsOptimised, isTransformableWithinSingleEdit, isUnique, isUniqueNoDataStructures, urlify}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class HRankUtilSpec extends AnyFlatSpec with should.Matchers {

  "urlify function" should "convert spaces to %20" in {
    val array = "Mr John Smith    ".toCharArray
    urlify(array, 13)
    array.mkString should be("Mr%20John%20Smith")

    "aaaa".toCharArray.length
  }

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

  "isPermutation function" should "return true when strings have the same characters" in {
    isPermutation("score", "cores") should be(true)
  }

  it should "return false where characters differ" in {
    isPermutation("soccer", "score") should be(false)
  }

  "isPermutationOfPalindrome function" should "return true when string can be rearranged to a palindrome" in {
    isPermutationOfPalindrome("Tact Coa") should be(true)
  }

  it should "return false when string cannot be rearranged to a palindrome" in {
    isPermutationOfPalindrome("Tact coax") should be(false)
  }

  "Constant-factor-optimised version of isPermutationOfPalindrome function" should "return true when string can be rearranged to a palindrome" in {
    isPermutationOfPalindromeConstantFactorsOptimised("Tact Coa") should be(true)
  }

  it should "return false when string cannot be rearranged to a palindrome" in {
    isPermutationOfPalindromeConstantFactorsOptimised("Tact coax") should be(false)
  }

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
