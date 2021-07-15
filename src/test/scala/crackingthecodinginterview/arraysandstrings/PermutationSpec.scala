package crackingthecodinginterview.arraysandstrings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Permutation._

class PermutationSpec extends AnyFlatSpec with should.Matchers {

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
