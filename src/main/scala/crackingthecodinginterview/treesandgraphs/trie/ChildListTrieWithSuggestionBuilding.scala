package crackingthecodinginterview.treesandgraphs.trie

private case class ChildListNodeWithSuggestionBuilding(override val storedChar: Option[Char])
  extends ListChildNodeStore[ChildListNodeWithSuggestionBuilding]
    with SuggestionBuilder {

  override protected def createNewNode(char: Char): ChildListNodeWithSuggestionBuilding =
    ChildListNodeWithSuggestionBuilding(Some(char))

  override def buildSuggestions(prefix: String): List[String] = {
    // Remove the last letter of the initial prefix passed so we don't re-add when we process the node itself,
    // then delegate to private method which can recurse in a consistent way.
    innerBuildSuggestions(prefix.slice(0, prefix.length - 1))
  }

  private def innerBuildSuggestions(prefix: String): List[String] = {
    storedChar.map { char =>
      val nextPrefix = prefix + char
      val childSuggestions = children.flatMap(_.innerBuildSuggestions(nextPrefix)).toList
      if (endsValidWord) childSuggestions :+ nextPrefix else childSuggestions
    }.getOrElse(List())
  }

}

object ChildListTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildListNodeWithSuggestionBuilding(None))

}
