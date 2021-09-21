package crackingthecodinginterview.arraysandstrings

object EditDistance {

  /** *
   * Work out whether we can transform the original string into the target by doing one (or zero) transformations,
   * which can be adding, removing or changing a single character.
   *
   * Note that this is likely to be *much* simpler than calculating edit distances of an arbitrary size;
   * because we don't have to consider the impact of doing multiple interacting edits
   * we can work out just by looking at the lengths of the two strings (O(1) in JVM lang's to get string length)
   * and know whether which class of single edit might work.
   *
   * @param string1 First string to check edit distance
   * @param string2 Second string to check edit distance
   * @return
   */
  def isTransformableWithinSingleEdit(string1: String, string2: String): Boolean = {
    // Identify the shorter and longer strings (O(1))
    // putting them in a known order means adding a character to shorter equates to removing from longer
    val (shorter, longer) = if (string1.length > string2.length) {
      (string2, string1)
    } else (string1, string2)
    // If length difference is greater than the amount of edits available (1) we definitely can't get there (O(1))
    if (longer.length - shorter.length > 1) {
      return false
    }

    object EditType extends Enumeration {
      type EditType = Value
      val ReplaceCharacter, AddCharacter = Value
    }

    // Constant time assignments
    val editType = if (longer.length == shorter.length) EditType.ReplaceCharacter else EditType.AddCharacter
    var (shorterIndex, longerIndex, editAlreadyUsed) = (0, 0, false)

    // O(n) where n is the *shorter* of the two lengths - everything in the loop is constant-time
    while (shorterIndex < shorter.length & longerIndex < longer.length) {
      val currentCharsMismatch = shorter(shorterIndex) != longer(longerIndex)
      if (currentCharsMismatch) {
        // Fail if we've already used up our one edit - otherwise we can ignore the non-match this once.
        if (editAlreadyUsed) {
          return false
        }
        editAlreadyUsed = true
      }
      shorterIndex += (if (currentCharsMismatch & editType == EditType.AddCharacter) 0 else 1)
      longerIndex += 1
    }
    true
  }

}
