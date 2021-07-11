package hrank

import hrank.HRankUtil.{isPermutation, isPermutationOfPalindrome, isPermutationOfPalindromeConstantFactorsOptimised, isUnique, isUniqueNoDataStructures, urlify}
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

}
