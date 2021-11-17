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
    if (maxNumberOfSuggestions.exists(_ <= 0)) return List()
    storedChar.map { char =>
      val nextPrefix = prefix + char
      var suggestions = if(endsValidWord) List(nextPrefix) else List()
      var remainingSuggestions = maxNumberOfSuggestions.map(s => s - (if(endsValidWord) 1 else 0))
      children
        // If we DON'T care exactly what order we use to pick suggestions, this sort is wasted
        .sortBy(_.storedChar.get)
        .foreach { child =>
          if (remainingSuggestions.forall(_ > 0)) {
            val childSuggestions = child.innerBuildSuggestions(nextPrefix, remainingSuggestions)
            val numChildSuggestions = remainingSuggestions.getOrElse(childSuggestions.length)
            suggestions = suggestions ::: childSuggestions.take(numChildSuggestions)
            remainingSuggestions = remainingSuggestions.map(_ - childSuggestions.length)
          }
        }
      suggestions
    }.getOrElse(List())
  }

}

object ChildListTrieWithSuggestionBuilding {

  def apply(): Trie = TrieWithSuggestionBuilding(ChildListNodeWithSuggestionBuilding(None))

}
