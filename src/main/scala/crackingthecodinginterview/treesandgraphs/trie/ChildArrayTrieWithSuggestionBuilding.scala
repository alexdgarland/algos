package crackingthecodinginterview.treesandgraphs.trie

private case class ChildArraySuggestionBuilderNode()
  extends ArrayChildNodeStore[ChildArraySuggestionBuilderNode]
    with SuggestionBuilder {

  protected override def createNewNode(): ChildArraySuggestionBuilderNode = ChildArraySuggestionBuilderNode()

  override def buildSuggestions(prefix: String): List[String] = {
    if (prefix.isEmpty) return List()
    val childSuggestions = children
      .zipWithIndex
      .flatMap { case (nodeOption, index) =>
        nodeOption.map { _.buildSuggestions(prefix + charFromIndex(index))}
          .getOrElse(List())
      }.toList
    if (endsValidWord) childSuggestions :+ prefix else childSuggestions
  }

}

object ChildArrayTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildArraySuggestionBuilderNode())

}
