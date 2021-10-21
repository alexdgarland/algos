package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

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
sealed trait TrieNode {

  val children: mutable.MutableList[Letter] = mutable.MutableList[Letter]()
  var endsValidWord: Boolean = false

  def existingChildWithChar(char: Char): Option[Letter] = children.find { _.char == char }

  def childForChar(char: Char): Letter = existingChildWithChar(char)
    .getOrElse {
      val newNode = Letter(char)
      children += newNode
      newNode
    }

}

case class StartOfWord() extends TrieNode

case class Letter(char: Char) extends TrieNode

class Trie {

  private val root = StartOfWord()

  private def traverseFromRoot(word: String, f: (Char, TrieNode) => TrieNode): TrieNode = {
    var currentNode: TrieNode = root
    word.toLowerCase().foreach { char => currentNode = f(char, currentNode) }
    currentNode
  }

  def add(word: String): Unit = traverseFromRoot(
    word,
    (char, node) => node.childForChar(char)
  ).endsValidWord = true

  def contains(word: String): Boolean = traverseFromRoot(
    word,
    (char, node) => node.existingChildWithChar(char).getOrElse(return false)
  ).endsValidWord

}
