package crackingthecodinginterview.treesandgraphs.trie

import scala.annotation.tailrec

private class TrieWithSuggestionStoring[N <: TrieNode[N] with SuggestionsStore](root: N) extends Trie {

  type TraversalActionWithOriginalWord = N => (Char, String) => Option[N]

  private def traverseFromRootWithOriginalWord
  (
    word: String,
    action: TraversalActionWithOriginalWord
  ): Option[N] = {

    @tailrec
    def inner
    (
      originalWord: String,
      remainingWord: String,
      nodeOption: Option[N],
      action: TraversalActionWithOriginalWord
    ): Option[N] = {
      if (remainingWord.isEmpty) nodeOption
      // This is effectively a flatMap but expanding to an explicit pattern-match allows tail recursion
      else nodeOption match {
        case None =>
          None
        case Some(node) =>
          val nextNode = action(node)(remainingWord.head.toLower, originalWord)
          inner(originalWord, remainingWord.tail, nextNode, action)
      }
    }

    inner(word.toLowerCase(), word, Some(root), action)
  }

  private def findNode(chars: String) = traverseFromRootWithOriginalWord(
    chars,
    node => (char, _) => node.getChild(char)
  )

  override protected def addValidated(word: String): Unit = traverseFromRootWithOriginalWord(
    word,
    node => (char, fullWord) => {
      val nodeOption = node.childOptionForChar(char)
      nodeOption.foreach(_.addSuggestion(fullWord))
      nodeOption
    }
  ).foreach(_.endsValidWord = true)

  override def contains(word: String): Boolean = findNode(word).exists(_.endsValidWord)

  override def suggestions(prefix: String): List[String] = findNode(prefix).map(_.getSuggestions).getOrElse(List())

}
