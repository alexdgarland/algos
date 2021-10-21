package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

sealed trait WordElement

case object StartOfWord extends WordElement

case class Letter(char: Char) extends WordElement

case object EndOfWord extends WordElement

/**
 * Using a generic n-ary style of node (with a list of children) has some pros and cons.
 *
 * If we use a fixed-size (26 elements for each of case-insensitive english chars) array for the children,
 * this will perform slightly better for densely populated trie (fast, consistent constant-time lookup),
 * but where more sparsely populated will waste a lot of memory - a collection with variable size can be smaller
 * but needs searching - also a set might be more appropriate than a list??
 *
 * Overall trie is a data structure where there are interesting trade-offs to be made
 * (rather than a single optimal solution) - this seems to be true of graphs more generally versus (e.g.)
 * linked-lists where there are no doubt some trade-offs but some level of best practice seems more applicable.
 */
case class TrieNode
(
  value: WordElement,
  children: mutable.MutableList[TrieNode] = mutable.MutableList[TrieNode]()
) {

  def existingChildWithChar(char: Char): Option[TrieNode] = {
    children.find { _.value match { case Letter(nodeCharacter) => nodeCharacter == char } }
  }

  def childForChar(char: Char): TrieNode = existingChildWithChar(char)
    .getOrElse {
      val newNode = TrieNode(Letter(char))
      children += newNode
      newNode
    }

  def endsWord: Boolean = children.contains(TrieNode(EndOfWord))

  def markEnd(): Unit = if (!endsWord) children += TrieNode(EndOfWord)

}

class Trie {

  private val root: TrieNode = TrieNode(StartOfWord)

  private def traverseFromRoot(word: String, f: (Char, TrieNode) => TrieNode): TrieNode = {
    var currentNode = root
    word.toLowerCase().foreach { char => currentNode = f(char, currentNode) }
    currentNode
  }

  def add(word: String): Unit = traverseFromRoot(
    word,
    (char, currentNode) => currentNode.childForChar(char)
  ).markEnd()

  def contains(word: String): Boolean = traverseFromRoot(
    word,
    (char, currentNode) => currentNode.existingChildWithChar(char).getOrElse(return false)
  ).endsWord

}
