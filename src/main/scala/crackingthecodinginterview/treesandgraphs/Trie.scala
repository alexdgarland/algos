package crackingthecodinginterview.treesandgraphs

/**
 * Contract for node type(s) that will implement level-by-level navigation within the Trie structure.
 */
trait TrieNode {

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

  var endsValidWord: Boolean = false

}

/**
 * Trie that can operate using any implementation of TrieNode.
 *
 * @param root TrieNode implementation that will sit at root, intermediating creation of and access to other nodes.
 */
case class Trie(root: TrieNode) {

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

}

/**
 * IMPLEMENTATIONS
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

  private def charIndex(char: Char) = char.toInt - ASCII_OFFSET

  private case class ChildArrayTrieNode() extends TrieNode {

    private val children: Array[Option[TrieNode]] = Array.tabulate(26)(_ => None)

    def existingChildWithChar(char: Char): Option[TrieNode] = children(charIndex(char))

    def childForChar(char: Char): TrieNode = existingChildWithChar(char)
      .getOrElse {
        val newNode = ChildArrayTrieNode()
        children(charIndex(char)) = Some(newNode)
        newNode
      }

  }

  def apply(): Trie = Trie(ChildArrayTrieNode())

}

object ChildListTrie {

  import scala.collection.mutable

  private sealed trait ChildListTrieNode extends TrieNode {

    private val children: mutable.MutableList[Letter] = mutable.MutableList[Letter]()

    def existingChildWithChar(char: Char): Option[TrieNode] = children.find { _.char == char }

    def childForChar(char: Char): TrieNode = existingChildWithChar(char)
      .getOrElse {
        val newNode = Letter(char)
        children += newNode
        newNode
      }

  }

  private case class StartOfWord() extends ChildListTrieNode

  private case class Letter(char: Char) extends ChildListTrieNode

  def apply(): Trie = Trie(StartOfWord())

}
