package crackingthecodinginterview.treesandgraphs.trie

/**
 * Top-level abstraction that exposes only the methods that would be used by a caller
 * in terms of adding/ checking/ retrieving strings.
 *
 * The node implementations are fully encapsulated and not indicated for direct usage.
 */
trait Trie {

  /**
   * Add a new word to the Trie.
   *
   * @param word Word to add.
   *             Must only contain standard ASCII letters, which will be store in a case-insensitive way.
   */
  def add(word: String): Unit

  /**
   * Check if word exists in the Trie.
   *
   * @param word Word to check for.
   *             Must only contain standard ASCII letters, which will be store in a case-insensitive way.
   *
   * @return
   */
  def contains(word: String): Boolean

  /**
   * Returns all possible words (from those stored in the Trie) that start with a given prefix.
   * Approx use case - auto-complete in UI context.
   *
   * @param prefix The prefix for which to return suggestions.
   * @param maxNumberOfSuggestions The maximum number of suggestions to return.
   *                               If less than the number available, the selection is made in alphabetical order.
   * @return
   */
  def suggestions(prefix: String, maxNumberOfSuggestions: Option[Int] = None): List[String]

}
