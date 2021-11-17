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
    storedChar.map { char =>
      val nextPrefix = prefix + char
      var suggestions = if(endsValidWord) List(nextPrefix) else List()
      children
        // If we DON'T care exactly what order we use to pick suggestions, this sort is wasted
        .sortBy(_.storedChar.get)
        .foreach { child =>
          val remainingSuggestions = maxNumberOfSuggestions.map(s => s - suggestions.length)
          if (!remainingSuggestions.contains(0)) {
            suggestions = suggestions ::: child.innerBuildSuggestions(nextPrefix, remainingSuggestions)
          }
        }
      suggestions
    }.getOrElse(List())
  }

}

object ChildListTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildListNodeWithSuggestionBuilding(None))

}
