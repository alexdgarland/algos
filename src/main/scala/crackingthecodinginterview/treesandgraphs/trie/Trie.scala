package crackingthecodinginterview.treesandgraphs.trie

/**
 * Top-level abstraction that exposes only the methods that would be used by a caller
 * in terms of adding/ checking/ retrieving strings.
 *
 * The node implementations are fully encapsulated and not indicated for direct usage.
 */
trait Trie {

  def add(word: String): Unit

  def contains(word: String): Boolean

  def suggestions(prefix: String): List[String]

}
