package crackingthecodinginterview.treesandgraphs.trie

import scala.collection.mutable

private trait TrieNode[N] {

  def addChild(char: Char): N

  def getChild(char: Char): Option[N]

  def childOptionForChar(char: Char): Option[N] = getChild(char) match {
    case existing@Some(_) => existing
    case None => Some(addChild(char))
  }

  var endsValidWord: Boolean = false

}

/*
NODE STORAGE IMPLEMENTATIONS

Where the Trie is expected to be densely populated (most characters present at each level),
array-based implementation may perform better as it can go directly to a fixed index (no iterative search needed).

However, for Tries which are expected to be more sparsely populated, some memory may be wasted and it may be
more possible to use a list - will use less memory (and not be too slow to search) when small.
 */

private trait ArrayChildNodeStore[N <: ArrayChildNodeStore[_]] extends TrieNode[N] {

  private val ASCII_OFFSET = 96

  protected def indexFromChar(char: Char): Int = char.toInt - ASCII_OFFSET

  protected def charFromIndex(index: Int): Char = (index + ASCII_OFFSET).toChar

  protected val children: Array[Option[N]] = Array.tabulate(26)(_ => None)

  protected def createNewNode(): N

  override def addChild(char: Char): N = {
    val newNode = createNewNode()
    children(indexFromChar(char)) = Some(newNode)
    newNode
  }

  override def getChild(char: Char): Option[N] = children(indexFromChar(char))

}

private trait ListChildNodeStore[N <: ListChildNodeStore[_]] extends TrieNode[N] {

  val storedChar: Option[Char] = None

  protected val children: mutable.MutableList[N] = mutable.MutableList[N]()

  protected def createNewNode(char: Char): N

  override def addChild(char: Char): N = {
    val newNode = createNewNode(char)
    children += newNode
    newNode
  }

  override def getChild(char: Char): Option[N] = children.find(_.storedChar.contains(char))

}

/*
SUGGESTION STRATEGIES

There are two obvious ways to do it:

- Recalculate on the fly each time by traversing down the sub-trie -
    that is more expensive at read-time but cheaper at write time and in terms of memory
- Save the full list of words at each prefix node -
    faster to read but will need updating on each write and could use a LOT of memory.

TODO (potentially): further optimisations could involve returning only a subset of the possible words:
  - Performance-focused - stop looking as soon as have found _n_ arbitrarily-selected (first available) matches.
  - Functionality-focused - could return the most likely/ appropriate words -
    but this would involve some depth of useful data around relative usage/ probability
    (which I guess we could kind of fake but not necessarily very useful relative to the impl complexity??)
    or maybe doing something like a full-on Markov chain (???) which is way out of scope for right now...
    Could also make it not lose its place in the Trie as new letters are submitted in a real-time text stream -
    but that could get fairly complex.

 */

private trait SuggestionBuilder {

  def buildSuggestions(prefix: String): List[String]

}

private trait SuggestionsStore {

  protected val suggestions: mutable.Set[String] = mutable.Set[String]()

  def addSuggestion(suggestion: String): Unit = suggestions.add(suggestion)

  def getSuggestions: List[String] = suggestions.toList

}


