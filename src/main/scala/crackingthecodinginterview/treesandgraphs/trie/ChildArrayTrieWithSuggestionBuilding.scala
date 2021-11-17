package crackingthecodinginterview.treesandgraphs.trie

private case class ChildArraySuggestionBuilderNode()
  extends ArrayChildNodeStore[ChildArraySuggestionBuilderNode]
    with SuggestionBuilder {

  protected override def createNewNode(): ChildArraySuggestionBuilderNode = ChildArraySuggestionBuilderNode()

  override def buildSuggestions(prefix: String, maxNumberOfSuggestions: Option[Int]): List[String] = {
    if (prefix.isEmpty || maxNumberOfSuggestions.contains(0)) return List()
    children
      .zipWithIndex
      .foldLeft(if(endsValidWord) List(prefix) else List()) {
        (suggestionsSoFar, nextArgs) =>
          val (childNode, index) = nextArgs
          val remainingSuggestionCount = maxNumberOfSuggestions.map(s => s - suggestionsSoFar.length)
          if(remainingSuggestionCount.contains(0)) {
            suggestionsSoFar
          }
          else {
            suggestionsSoFar :::
              childNode
                .map(_.buildSuggestions(prefix + charFromIndex(index), remainingSuggestionCount))
                .getOrElse(List())
          }
      }
  }
}

object ChildArrayTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildArraySuggestionBuilderNode())

}
