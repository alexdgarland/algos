package crackingthecodinginterview.treesandgraphs.trie

import scala.annotation.tailrec

/**
 * Trie implementation that can operate using any passed implementation of NodeWithSuggestionBuilding.
 *
 * @param root TrieImplNode implementation that will sit at root, intermediating creation of and access to other nodes.
 */
private case class TrieWithSuggestionBuilding[N <: TrieNode[N] with SuggestionBuilder](root: N) extends Trie {

  type TraversalAction = N => Char => Option[N]

  private def traverseFromRoot(word: String, action: TraversalAction): Option[N] = {

    @tailrec
    def inner(word: String, nodeOption: Option[N], action: TraversalAction): Option[N] = {
      if (word.isEmpty) nodeOption
      // This is effectively a flatMap but expanding to an explicit pattern-match allows tail recursion
      else nodeOption match {
        case None => None
        case Some(node) => inner(word.tail, action(node)(word.head.toLower), action)
      }
    }

    inner(word, Some(root), action)
  }

  private def findNode(chars: String) = traverseFromRoot(chars, _.getChild)

  /**
   * Add a new word to the Trie.
   *
   * @param word Word to add.
   *             Must only contain standard ASCII letters, which will be store in a case-insensitive way.
   */
  def addValidated(word: String): Unit = traverseFromRoot(word, _.childOptionForChar).foreach(_.endsValidWord = true)

  /**
   * Check if word exists in the Trie.
   *
   * @param word Word to check for.
   *             Must only contain standard ASCII letters, which will be checked for in a case-insensitive way.
   * @return
   */
  def contains(word: String): Boolean = findNode(word).exists(_.endsValidWord)

  def suggestions(prefix: String): List[String] = findNode(prefix)
    .map(_.buildSuggestions(prefix))
    .getOrElse(List())

}
