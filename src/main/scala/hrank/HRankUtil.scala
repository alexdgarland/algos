package hrank

import scala.collection.mutable.{Map => MutableMap}

object HRankUtil {

  def isUnique(string: String): Boolean = {
    val observedChars = MutableMap[Char, Unit]()
    // O(n) (best case would be better if for example first two characters are the same)
    string.foreach{ char =>
      // Operations against hashmap are constant-time
      if (observedChars.contains(char)) { return false }
      observedChars += (char -> ())
    }
    true
  }

  def isUniqueNoDataStructures(string: String): Boolean = {
    // This sort makes it O(n log n) (Scala uses TimSort)
    val sorted = string.sorted
    (0 until (sorted.length - 1)).foreach { i =>
      if (sorted(i) == sorted(i + 1)) {
        return false
      }
    }
    true
  }

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

  def urlify(chars: Array[Char], trueLength: Int): Unit = {
    // O(n) - could do marginally more efficiently (?) in terms of constant factors
    // by walking backwards through the array and starting to count spaces after hitting a real char
    // but it wouldn't change the big-O
    var remainingSpaceCount = chars.take(trueLength).count(_ == ' ')
    // O(n) (nothing within the loop takes anything more than constant time)
    (trueLength -1 to 0 by -1).foreach { i =>
      val offset = i + (remainingSpaceCount * 2)
      if (chars(i) == ' ') {
        chars(offset) = '0'
        chars(offset - 1) = '2'
        chars(offset - 2) = '%'
        remainingSpaceCount -= 1
      }
      else {
        chars(offset) = chars(i)
      }
    }
  }

}
