package crackingthecodinginterview.treesandgraphs.trie

/**
 * Top-level abstraction that exposes only the methods that would be used by a caller
 * in terms of adding/ checking/ retrieving strings.
 *
 * The node implementations are fully encapsulated and not indicated for direct usage.
 */
trait Trie {

  private def isASCIILetter(char: Char): Boolean = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

  private def validateCharacters(word: String): Unit = {
    val badChars = word.filter(char => !isASCIILetter(char))
    if (badChars.nonEmpty) throw new IllegalArgumentException(
      s"Cannot add characters other than ASCII letters to trie - found ${badChars.mkString("'", "', '", "'")}."
    )
  }

  protected def addValidated(word: String): Unit

  def add(word: String): Unit = {
    validateCharacters(word)
    addValidated(word)
  }

  def contains(word: String): Boolean

  /**
   * Returns all possible words (from those stored in the Trie) that start with a given prefix.
   * Approx use case - auto-complete in UI context.
   *
   * There are two obvious ways to do it:
   *
   *    - Recalculate on the fly each time by traversing down the sub-trie -
   *      that is more expensive at read-time but cheaper at write time and in terms of memory
   *
   *    - Save the full list of words at each prefix node -
   *      faster to read but will need updating on each write and could use a LOT of memory.
   *
   * TODO (potentially): further optimisations could involve returning only a subset of the possible words:
   *    - Performance-focused
   *      Stop looking as soon as have found _n_ arbitrarily-selected (first available) matches.
   *    - Functionality-focused
   *      Could return the most likely/ appropriate words -
   *      but this would involve some depth of useful data around relative usage/ probability
   *      (which I guess we could kind of fake but not necessarily very useful relative to the impl complexity??)
   *      or maybe doing something like a full-on Markov chain (???) which is way out of scope for right now...
   *      Could also make it not lose its place in the Trie as new letters are submitted in a real-time text stream -
   *      but that could get fairly complex.
   */
  def suggestions(prefix: String): List[String]

}
