package crackingthecodinginterview.treesandgraphs

/**
 * Top-level abstraction that exposes only the methods that would be used by a caller
 * in terms of adding/ checking/ retrieving strings.
 *
 * The node implementations are fully encapsulated and not indicated for direct usage.
 */
trait Trie {

  def add(word: String): Unit

  def contains(word: String): Boolean

  def suggestions(prefix: String): List[String]

}

/**
 * Contract for node type(s) that will implement level-by-level navigation within the Trie structure.
 */
private sealed trait TrieNode {

  /**
   * Return Some(node) if exists for a given character, None if it doesn't.
   *
   * @param char Character the node needs to match.
   * @return
   */
  def existingChildWithChar(char: Char): Option[TrieNode]

  /**
   * Return existing node that matches a given character if it exists, otherwise create, insert and return a new node.
   * @param char Character the node needs to match.
   * @return
   */
  def childForChar(char: Char): TrieNode

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

/**
 * Trie implementation that can operate using any implementation of TrieNode.
 *
 * @param root TrieNode implementation that will sit at root, intermediating creation of and access to other nodes.
 */
private case class TrieImpl(root: TrieNode) extends Trie {

  // TODO - should refactor, there are a couple of hacky-ish things here, the use of mutable state internally
  //  and (more importantly) the fact that some of the functions passed have to do getOrElse(return something).
  //  I believe it should be possible to do recursively,
  //  return an Option and itself terminate early returning None where needed,
  //  letting the caller handle the None case directly rather than via a messy callback.
  private def traverseFromRoot(word: String, f: (Char, TrieNode) => TrieNode): TrieNode = {
    var currentNode: TrieNode = root
    word.toLowerCase().foreach { char => currentNode = f(char, currentNode) }
    currentNode
  }

  /**
   * Add a new word to the Trie.
   *
   * @param word Word to add.
   *             Must only contain standard ASCII letters, which will be store in a case-insensitive way.
   */
  def add(word: String): Unit = traverseFromRoot(
    word,
    (char, node) => node.childForChar(char)
  ).endsValidWord = true

  /**
   * Check if word exists in the Trie.
   *
   * @param word Word to check for.
   *             Must only contain standard ASCII letters, which will be checked for in a case-insensitive way.
   * @return
   */
  def contains(word: String): Boolean = traverseFromRoot(
    word,
    (char, node) => node.existingChildWithChar(char).getOrElse(return false)
  ).endsValidWord


  /**
   *  Returns all possible words (from those stored in the Trie) that start with a given prefix.
   *  Approx use case - auto-complete in UI context.
   *
   *  There are two obvious ways to do it:
   *
   *    - Recalculate on the fly each time by traversing down the sub-trie -
   *      that is more expensive at read-time but cheaper at write time and in terms of memory
   *      That is what is currently implemented here.
   *
   *    - TODO as a potential alternative:
   *        Save the full list of words at each prefix node -
   *        faster to read but will need updating on each write and could use a LOT of memory.
   *
   *  TODO (potentially): further optimisations could involve returning only a subset of the possible words:
   *    - Performance-focused
   *      Stop looking as soon as have found _n_ arbitrarily-selected (first available) matches.
   *    - Functionality-focused
   *      Could return the most likely/ appropriate words -
   *      but this would involve some depth of useful data around relative usage/ probability
   *      (which I guess we could kind of fake but not necessarily very useful relative to the impl complexity??)
   *      or maybe doing something like a full-on Markov chain (???) which is way out of scope for right now...
   *   Could also make it not lose its place in the Trie as new letters are submitted in a real-time text stream -
   *   but that could get fairly complex.
   */
  def suggestions(prefix: String): List[String] = {
    val prefixEndNode = traverseFromRoot(
      prefix,
      (char, node) => node.existingChildWithChar(char).getOrElse(return List())
    )
    prefixEndNode.buildSuggestions(prefix)
  }

}

/**
 * NODE IMPLEMENTATIONS
 *
 * Where the Trie is expected to be densely populated (most characters present at each level),
 * ChildArrayTrie may perform better as it can go directly to a fixed index (no iterative search needed).
 *
 * However, for Tries which are expected to be more sparsely populated, some memory may be wasted
 * and it may be more possible to use ChildListTrie -
 * the lists will use less memory (and not be too slow to search) when small.
 *
 */

object ChildArrayTrie {

  private val ASCII_OFFSET = 96

  private def indexFromChar(char: Char) = char.toInt - ASCII_OFFSET

  private def charFromIndex(index: Int) = (index + ASCII_OFFSET).toChar

  private case class ChildArrayTrieNode() extends TrieNode {

    private val children: Array[Option[TrieNode]] = Array.tabulate(26)(_ => None)

    override def existingChildWithChar(char: Char): Option[TrieNode] = children(indexFromChar(char))

    override def childForChar(char: Char): TrieNode = existingChildWithChar(char)
      .getOrElse {
        val newNode = ChildArrayTrieNode()
        children(indexFromChar(char)) = Some(newNode)
        newNode
      }

    override def buildSuggestions(prefix: String): List[String] = {
      if (prefix.isEmpty) return List()
      val childSuggestions = children
        .zipWithIndex
        .flatMap { case(nodeOption, index) =>
          nodeOption
            .map { _.buildSuggestions(prefix + charFromIndex(index)) }
            .getOrElse(List())
        }.toList
      if(endsValidWord) childSuggestions :+ prefix else childSuggestions
    }
  }

  def apply(): Trie = TrieImpl(ChildArrayTrieNode())

}

object ChildListTrie {

  import scala.collection.mutable

  private sealed trait ChildListTrieNode extends TrieNode {

    protected val children: mutable.MutableList[Letter] = mutable.MutableList[Letter]()

    override def existingChildWithChar(char: Char): Option[TrieNode] = children.find { _.char == char }

    override def childForChar(char: Char): TrieNode = existingChildWithChar(char)
      .getOrElse {
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
      innerBuildSuggestions(prefix.slice(0, prefix.length -1))
    }

    private def innerBuildSuggestions(prefix: String): List[String] = {
      val nextPrefix = prefix + char
      val childSuggestions = children.flatMap(_.innerBuildSuggestions(nextPrefix)).toList
      if(endsValidWord) childSuggestions :+ nextPrefix else childSuggestions
    }

  }

  def apply(): Trie = TrieImpl(StartOfWord())

}
