package crackingthecodinginterview.treesandgraphs.trie

/**
 * Contract for node type(s) that will implement level-by-level navigation within the TrieImpl structure.
 */
private trait TrieImplNode {

  /**
   * Return Some(node) if exists for a given character, None if it doesn't.
   *
   * @param char Character the node needs to match.
   * @return
   */
  def existingChildWithChar(char: Char): Option[TrieImplNode]

  /**
   * Create, insert and return a new node for a given character.
   *
   * @param char Character the node needs to match.
   * @return
   */
  def newChild(char: Char): TrieImplNode

  def childOptionForChar(char: Char): Option[TrieImplNode] = existingChildWithChar(char) match {
    case existing@Some(_) => existing
    case None => Some(newChild(char))
  }

  /**
   * Indicates whether the set of letters negotiated thus far constitutes a full valid word.
   * This does not preclude the letters also forming the first part of a longer word
   * (with the rest represented by additional child nodes).
   */
  var endsValidWord: Boolean = false

  /**
   * Build out a list of suggestions
   * by *adding* the supplied prefix to the sequence of letters found via downward recursion.
   *
   * @param prefix Prefix to be added to words.
   * @return
   */
  def buildSuggestions(prefix: String): List[String]

}
