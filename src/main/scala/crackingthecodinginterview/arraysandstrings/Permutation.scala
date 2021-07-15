package crackingthecodinginterview.arraysandstrings

import scala.collection.mutable.{Map => MutableMap}

object Permutation {

  // O(n)
  private def getCharFrequencies(s: String): MutableMap[Char, Int] = {
    val map = MutableMap[Char, Int]()
    // Map insertion is a constant-time operation, for each of n chars in string
    s.foreach { char => map += (char -> (map.getOrElse(char, 0) + 1)) }
    map
  }

  // O(n)
  def isPermutation(s1: String, s2: String): Boolean = {
    // getCharFrequencies is O(n) (and is called twice)
    // Map comparison is O(n)
    // (in practice likely slightly lower, assuming duplicate chars (which make map size < n) and-or early exit on non-match)
    getCharFrequencies(s1) == getCharFrequencies(s2)
  }

  // O(n)
  def isPermutationOfPalindrome(string: String): Boolean = {
    // The lower-casing and space removal are each straightforward passes through each char so O(n)
    // Space complexity might be reduced by not creating additional strings (at the cost of more complex and non-shared code)
    // and might also reduce the number of passes
    // (but that doesn't affect time *complexity* - it's just a constant-factor optimisation)
    val normalised = string
      .toLowerCase()
      .replaceAll("\\s", "")

    // getCharFrequencies is O(n)
    getCharFrequencies(normalised)
      // Iterating through the dict is O(n), likely slightly less in practice
      .count(kv => kv._2 % 2 == 1) <= 1
  }

  // O(n)
  def isPermutationOfPalindromeConstantFactorsOptimised(string: String): Boolean = {
    val map = MutableMap[Char, Int]()
    string.foreach { char =>
      if (char != ' ') {
        val lower = char.toLower
        map += (lower -> (map.getOrElse(lower, 0) + 1))
      }
    }
    map.count(kv => kv._2 % 2 == 1) <= 1
  }

}
