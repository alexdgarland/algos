package crackingthecodinginterview.treesandgraphs.trie

private case class ChildListNodeWithSuggestionStoring(override val storedChar: Option[Char])
  extends ListChildNodeStore[ChildListNodeWithSuggestionStoring]
    with SuggestionsStore {

  override def createNewNode(char: Char): ChildListNodeWithSuggestionStoring =
    ChildListNodeWithSuggestionStoring(Some(char))

}

object ChildListTrieWithSuggestionStoring {

  def apply(): Trie = new TrieWithSuggestionStoring(ChildListNodeWithSuggestionStoring(None))

}
