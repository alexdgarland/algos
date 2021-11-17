package crackingthecodinginterview.treesandgraphs.trie

import scala.annotation.tailrec

private trait TrieOperations[N <: TrieNode[N]] extends Trie {

  val root: N

  type TraversalAction = N => Char => Option[N]

  override def add(word: String): Unit = {
    validateCharacters(word)
    traverseFromRoot(
      word,
      traversalActionForAdd(word)
    ).foreach(_.endsValidWord = true)
  }

  override def contains(word: String): Boolean = {
    validateCharacters(word)
    findNode(word).exists(_.endsValidWord)
  }

  override def suggestions(prefix: String, maxNumberOfSuggestions: Option[Int]): List[String] =
    findNode(prefix)
    .map(suggestionsFromNode(_, prefix, maxNumberOfSuggestions))
    .getOrElse(List())

  protected def traverseFromRoot(word: String, action: TraversalAction): Option[N] = {

    @tailrec
    def inner(word: String, nodeOption: Option[N], action: TraversalAction): Option[N] = {
      if (word.isEmpty) nodeOption
      // This is effectively a flatMap but expanding to an explicit pattern-match allows tail recursion
      else nodeOption match {
        case None => None
        case Some(node) => inner(word.tail, action(node)(word.head.toLower), action)
      }
    }

    inner(word, Some(root), action)
  }

  protected def traversalActionForAdd(word: String): TraversalAction

  protected def findNode(chars: String): Option[N] = traverseFromRoot(chars, _.getChild)

  protected def suggestionsFromNode(node: N, prefix: String, maxNumberOfSuggestions: Option[Int]): List[String]

  private def isASCIILetter(char: Char): Boolean = (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z')

  private def validateCharacters(word: String): Unit = {
    val badChars = word.filter(char => !isASCIILetter(char))
    if (badChars.nonEmpty) throw new IllegalArgumentException(
      s"Words in trie cannot contain characters other than ASCII letters - found ${badChars.mkString("'", "', '", "'")}."
    )
  }

}
