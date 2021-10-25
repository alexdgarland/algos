package crackingthecodinginterview.treesandgraphs.trie

import scala.annotation.tailrec

private object char {

  implicit class CharProperties(val ch: Char) extends AnyVal {
    def isASCIILetter: Boolean = (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z')
  }

}

/**
 * Trie implementation that can operate using any passed implementation of TrieImplNode.
 *
 * Where the Trie is expected to be densely populated (most characters present at each level),
 * ChildArrayTrie may perform better as it can go directly to a fixed index (no iterative search needed).
 *
 * However, for Tries which are expected to be more sparsely populated, some memory may be wasted and it may be
 * more possible to use ChildListTrie - the lists will use less memory (and not be too slow to search) when small.
 *
 * @param root TrieImplNode implementation that will sit at root, intermediating creation of and access to other nodes.
 */
private case class TrieImpl(root: TrieImplNode) extends Trie {

  type TraversalAction = TrieImplNode => Char => Option[TrieImplNode]

  private def traverseFromRoot(word: String, action: TraversalAction): Option[TrieImplNode] = {

    @tailrec
    def inner(word: String, nodeOption: Option[TrieImplNode], action: TraversalAction): Option[TrieImplNode] = {
      if (word.isEmpty) nodeOption
      // This is effectively a flatMap but expanding to an explicit pattern-match allows tail recursion
      else nodeOption match {
        case None => None
        case Some(node) => inner(word.tail, action(node)(word.head.toLower), action)
      }
    }

    inner(word, Some(root), action)
  }

  private def findNode(chars: String) = traverseFromRoot(chars, _.existingChildWithChar)

  private def validateCharacters(word: String): Unit = {
    import char.CharProperties
    val badChars = word.filter(char => !char.isASCIILetter)
    if (badChars.nonEmpty) throw new IllegalArgumentException(
      s"Cannot add characters other than ASCII letters to trie - found ${badChars.mkString("'", "', '", "'")}."
    )
  }

  /**
   * Add a new word to the Trie.
   *
   * @param word Word to add.
   *             Must only contain standard ASCII letters, which will be store in a case-insensitive way.
   */
  def add(word: String): Unit = {
    validateCharacters(word)
    traverseFromRoot(word, _.childOptionForChar).foreach(_.endsValidWord = true)
  }

  /**
   * Check if word exists in the Trie.
   *
   * @param word Word to check for.
   *             Must only contain standard ASCII letters, which will be checked for in a case-insensitive way.
   * @return
   */
  def contains(word: String): Boolean = findNode(word).exists(_.endsValidWord)

  /**
   * Returns all possible words (from those stored in the Trie) that start with a given prefix.
   * Approx use case - auto-complete in UI context.
   *
   * There are two obvious ways to do it:
   *
   *    - Recalculate on the fly each time by traversing down the sub-trie -
   *      that is more expensive at read-time but cheaper at write time and in terms of memory
   *      That is what is currently implemented here.
   *
   *    - TODO as a potential alternative:
   *      Save the full list of words at each prefix node -
   *      faster to read but will need updating on each write and could use a LOT of memory.
   *
   * TODO (potentially): further optimisations could involve returning only a subset of the possible words:
   *    - Performance-focused
   *      Stop looking as soon as have found _n_ arbitrarily-selected (first available) matches.
   *    - Functionality-focused
   *      Could return the most likely/ appropriate words -
   *      but this would involve some depth of useful data around relative usage/ probability
   *      (which I guess we could kind of fake but not necessarily very useful relative to the impl complexity??)
   *      or maybe doing something like a full-on Markov chain (???) which is way out of scope for right now...
   *      Could also make it not lose its place in the Trie as new letters are submitted in a real-time text stream -
   *      but that could get fairly complex.
   */
  def suggestions(prefix: String): List[String] = findNode(prefix)
    .map(_.buildSuggestions(prefix))
    .getOrElse(List())

}
