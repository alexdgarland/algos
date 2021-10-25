package crackingthecodinginterview.treesandgraphs.trie

import scala.collection.mutable

private sealed trait ChildListTrieNode extends TrieImplNode {

  protected val children: mutable.MutableList[Letter] = mutable.MutableList[Letter]()

  override def existingChildWithChar(char: Char): Option[TrieImplNode] = children.find(_.char == char)

  override def newChild(char: Char): TrieImplNode = {
    val newNode = Letter(char)
    children += newNode
    newNode
  }

}

private case class StartOfWord() extends ChildListTrieNode {

  override def buildSuggestions(prefix: String): List[String] = List()

}

private case class Letter(char: Char) extends ChildListTrieNode {

  override def buildSuggestions(prefix: String): List[String] = {
    // Remove the last letter of the initial prefix passed so we don't re-add when we process the node itself,
    // then delegate to private method which can recurse in a consistent way.
    innerBuildSuggestions(prefix.slice(0, prefix.length - 1))
  }

  private def innerBuildSuggestions(prefix: String): List[String] = {
    val nextPrefix = prefix + char
    val childSuggestions = children.flatMap(_.innerBuildSuggestions(nextPrefix)).toList
    if (endsValidWord) childSuggestions :+ nextPrefix else childSuggestions
  }

}

object ChildListTrie {

  def apply(): Trie = TrieImpl(StartOfWord())

}
