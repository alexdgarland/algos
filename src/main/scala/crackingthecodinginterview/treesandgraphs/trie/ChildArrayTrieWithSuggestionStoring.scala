package crackingthecodinginterview.treesandgraphs.trie

private case class ChildArrayNodeWithSuggestionStoring()
  extends ArrayChildNodeStore[ChildArrayNodeWithSuggestionStoring]
    with SuggestionsStore {
  override protected def createNewNode(): ChildArrayNodeWithSuggestionStoring =
    ChildArrayNodeWithSuggestionStoring()
}

object ChildArrayTrieWithSuggestionStoring {

  def apply(): Trie = new TrieWithSuggestionStoring(ChildArrayNodeWithSuggestionStoring())

}
