package crackingthecodinginterview.treesandgraphs.trie

/**
 * Trie implementation that can operate using any node type implementing SuggestionBuilder.
 *
 * @param root Node implementation that will sit at root, intermediating creation of and access to other nodes.
 */
private case class TrieWithSuggestionBuilding[N <: TrieNode[N] with SuggestionBuilder](root: N)
  extends TrieOperations[N] {

  override protected def traversalActionForAdd(word: String): TraversalAction = _.childOptionForChar

  override protected def suggestionsFromNode(node: N, prefix: String): List[String] = node.buildSuggestions(prefix)

}
