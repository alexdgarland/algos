package crackingthecodinginterview.arraysandstrings

import scala.collection.mutable.{Map => MutableMap}

object IsUnique {

  def isUnique(string: String): Boolean = {
    val observedChars = MutableMap[Char, Unit]()
    // O(n) (best case would be better if for example first two characters are the same)
    string.foreach { char =>
      // Operations against hashmap are constant-time
      if (observedChars.contains(char)) {
        return false
      }
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

}
