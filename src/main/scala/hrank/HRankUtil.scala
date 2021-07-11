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

  /***
   *  Work out whether we can transform the original string into the target by doing one (or zero) transformations,
   *  which can be adding, removing or changing a single character.
   *
   *  Note that this is likely to be *much* simpler than calculating edit distances of an arbitrary size;
   *  because we don't have to consider the impact of doing multiple interacting edits
   *  we can work out just by looking at the lengths of the two strings (O(1) in JVM lang's to get string length)
   *  and know whether which class of single edit might work.
   *
   * @param original
   * @param target
   * @return
   */
  def isTransformableWithinSingleEdit(original: String, target: String): Boolean = {
    // Some early exit conditions - can these in some way form a base case for recursion?
    // String equality comparison is O(n)
    if (original == target) { return true }
    val stringLengthDiff = original.length - target.length
    // If length difference is greater than the amount of edits available (1) we definitely can't get there
    if (stringLengthDiff.abs > 1) { return false }

    // We can now switch in terms of the direction of the difference (if any) in length
    // The logic to loop over chars here can maybe be made common in some way -
    // but let's get it working first and then maybe refactor
    // All of the following iterate through the string once, so are in O(n)
    var editUsed = false

    if(stringLengthDiff == 0) {
      // same length - try replacing characters
      (0 until original.length).foreach { i =>
        if(original(i) != target(i)) {
          if(editUsed) { return false }
          editUsed = true
        }
      }
    }

    if(stringLengthDiff == 1) {
      // original is longer - try removing characters (or rather skipping over a character in original)
      var originalSkip = 0
      // The loop range here is different so take care if trying to make the code more generic
      (0 until original.length -1).foreach { i =>
        if(original(i + originalSkip) != target(i)) {
          if(editUsed) { return false }
          originalSkip = 1
          editUsed = true
        }
      }
    }

    if(stringLengthDiff == -1) {
      // original is shorter - try adding characters (implemented by skipping over a character in target)
      var targetSkip = 0
      (0 until original.length).foreach { i =>
        if(original(i) != target(i  + targetSkip)) {
          if(editUsed) { return false }
          targetSkip = 1
          editUsed = true
        }
      }
    }
    true
  }

}
