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
   *             TODO - should probably also validate this on the way in and handle in a controlled way if it fails.
   *
   * @return
   */
  def contains(word: String): Boolean

  /**
   * Returns all possible words (from those stored in the Trie) that start with a given prefix.
   * Approx use case - auto-complete in UI context.

   * @param prefix The prefix for which to return suggestions.
   * @return
   */
  def suggestions(prefix: String): List[String]

}
