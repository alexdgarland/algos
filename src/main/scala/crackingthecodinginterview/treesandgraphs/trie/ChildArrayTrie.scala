package crackingthecodinginterview.treesandgraphs.trie

object ChildArrayTrie {

  private val ASCII_OFFSET = 96

  private def indexFromChar(char: Char) = char.toInt - ASCII_OFFSET

  private def charFromIndex(index: Int) = (index + ASCII_OFFSET).toChar

  private case class ChildArrayTrieNode() extends TrieImplNode {

    private val children: Array[Option[TrieImplNode]] = Array.tabulate(26)(_ => None)

    override def existingChildWithChar(char: Char): Option[TrieImplNode] = children(indexFromChar(char))

    override def newChild(char: Char): TrieImplNode = {
      val newNode = ChildArrayTrieNode()
      children(indexFromChar(char)) = Some(newNode)
      newNode
    }

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

  def apply(): Trie = TrieImpl(ChildArrayTrieNode())

}
