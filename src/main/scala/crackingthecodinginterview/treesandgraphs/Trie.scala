package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

/**
 * Can we do letters as 26 individual singletons (enums?) for reuse?
 */
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
)

/**
 * Code here is a bit messy and could undoubtedly do with some factoring out of shared parts
 * as well as the other changes mentioned above.
 */
class Trie {

  private val root: TrieNode = TrieNode(StartOfWord)

  def add(word: String): Unit = {
    var currentNode = root
    word.toLowerCase().foreach { wordCharacter =>
      currentNode =
        currentNode.children.find {
          _.value match {
            case Letter(nodeCharacter) => nodeCharacter == wordCharacter
            case _ => false
          }
        }.getOrElse {
          val newLetter = TrieNode(Letter(wordCharacter))
          currentNode.children += newLetter
          newLetter
        }
    }
    if(!currentNode.children.contains(TrieNode(EndOfWord))) {
      currentNode.children += TrieNode(EndOfWord)
    }
  }

  def contains(word: String): Boolean = {
    var currentNode = root
    word.toLowerCase().foreach { wordCharacter =>
      currentNode.children.find {
        _.value match {
          case Letter(nodeCharacter) => nodeCharacter == wordCharacter
          case _ => false
        }
    } match {
        case None => return false
        case Some(foundNode) => currentNode = foundNode
      }
    }
    currentNode.children.contains(TrieNode(EndOfWord))
  }

}
