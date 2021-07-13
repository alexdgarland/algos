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
   * @param string1
   * @param string2
   * @return
   */
  def isTransformableWithinSingleEdit(string1: String, string2: String): Boolean = {
    // Identify the shorter and longer strings (O(1))
    // putting them in a known order means adding a character to shorter equates to removing from longer
    val (shorter, longer) = if(string1.length > string2.length) { (string2, string1) } else (string1, string2)
    // If length difference is greater than the amount of edits available (1) we definitely can't get there (O(1))
    if (longer.length - shorter.length > 1) { return false }

    object EditType extends Enumeration {
      type EditType = Value
      val ReplaceCharacter, AddCharacter = Value
    }

    // Constant time assignments
    val editType = if(longer.length == shorter.length) EditType.ReplaceCharacter else EditType.AddCharacter
    var (shorterIndex, longerIndex, editAlreadyUsed) = (0, 0, false)

    // O(n) where n is the *shorter* of the two lengths - everything in the loop is constant-time
    while (shorterIndex < shorter.length & longerIndex < longer.length) {
      val currentCharsMismatch = shorter(shorterIndex) != longer(longerIndex)
      if (currentCharsMismatch) {
        // Fail if we've already used up our one edit - otherwise we can ignore the non-match this once.
        if (editAlreadyUsed) { return false }
        editAlreadyUsed = true
      }
      shorterIndex += (if(currentCharsMismatch & editType == EditType.AddCharacter) 0 else 1)
      longerIndex += 1
    }
    true
  }

}
