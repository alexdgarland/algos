package crackingthecodinginterview.treesandgraphs.trie

private case class ChildListNodeWithSuggestionBuilding(override val storedChar: Option[Char])
  extends ListChildNodeStore[ChildListNodeWithSuggestionBuilding]
    with SuggestionBuilder {

  override protected def createNewNode(char: Char): ChildListNodeWithSuggestionBuilding =
    ChildListNodeWithSuggestionBuilding(Some(char))

  override def buildSuggestions(prefix: String, maxNumberOfSuggestions: Option[Int]): List[String] = {
    // Remove the last letter of the initial prefix passed so we don't re-add when we process the node itself,
    // then delegate to private method which can recurse in a consistent way.
    innerBuildSuggestions(prefix.dropRight(1), maxNumberOfSuggestions)
  }

  private def innerBuildSuggestions(prefix: String, maxNumberOfSuggestions: Option[Int]): List[String] = {
    if (maxNumberOfSuggestions.contains(0)) return List()
    storedChar match {
      case None =>
        List()
      case Some(char) =>
        val nextPrefix = prefix + char
        children
          .sortBy(_.storedChar.get)   // If we DON'T care what order we use to pick suggestions, this sort is wasted
          .foldLeft(if(endsValidWord) List(nextPrefix) else List()) {
            (suggestionsSoFar, childNode) =>
              val remainingSuggestionCount = maxNumberOfSuggestions.map(s => s - suggestionsSoFar.length)
              if(remainingSuggestionCount.contains(0)) {
                suggestionsSoFar
              }
              else {
                suggestionsSoFar ::: childNode.innerBuildSuggestions(nextPrefix, remainingSuggestionCount)
              }
          }
    }
  }

}

object ChildListTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildListNodeWithSuggestionBuilding(None))

}
