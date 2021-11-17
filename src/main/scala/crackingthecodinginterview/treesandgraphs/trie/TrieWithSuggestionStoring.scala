package crackingthecodinginterview.treesandgraphs.trie

/**
 * Trie implementation that can operate using any node type implementing SuggestionsStore.
 *
 * @param root Node implementation that will sit at root, intermediating creation of and access to other nodes.
 */
private case class TrieWithSuggestionStoring[N <: TrieNode[N] with SuggestionsStore](override val root: N)
  extends TrieOperations[N] {

  override protected def traversalActionForAdd(word: String): TraversalAction =
    node => char => {
      val nodeOption = node.childOptionForChar(char)
      nodeOption.foreach(_.addSuggestion(word.toLowerCase()))
      nodeOption
    }

  override protected def suggestionsFromNode
  (
    node: N,
    prefix: String,
    maxNumberOfSuggestions: Option[Int]
  ): List[String] = node.getSuggestions(maxNumberOfSuggestions)

}
