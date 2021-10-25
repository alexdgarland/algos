package crackingthecodinginterview.treesandgraphs.trie

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait SharedTrieBehaviourTests {
  this: AnyFlatSpec with should.Matchers =>

  def defaultTrie(newTrie: => Trie): Unit = {

    def testTrie = {
      val trie = newTrie
      trie.add("Hello")
      trie.add("world")
      trie.add("hells")
      trie.add("hellscape")
      trie.add("hellenistic")
      trie
    }

    it should "throw an error on trying to add a word with characters other than basic English letters" in {
      val exception = intercept[IllegalArgumentException] {
        newTrie.add("Hello!?!")
      }
      exception.getMessage should be("Cannot add characters other than ASCII letters to trie - found '!', '?', '!'.")
    }

    it should "identify word that is contained" in {
      testTrie.contains("Hello") should be(true)
    }

    it should "identify word that is contained in a case-insensitive way" in {
      testTrie.contains("hello") should be(true)
    }

    it should "identify word that is not contained" in {
      testTrie.contains("again") should be(false)
    }

    it should "identify word that is not contained but is a subset of existing word as not contained" in {
      testTrie.contains("Hell") should be(false)
    }

    it should "identify word that is not contained but is a superset of existing word as not contained" in {
      testTrie.contains("Helloooo") should be(false)
    }

    it should "not offer list of suggestions for empty prefix" in {
      testTrie.suggestions("") should be(List())
    }

    it should "offer list of suggestions for a prefix which matches multiple words" in {
      testTrie.suggestions("hell").sorted should be(List("hellenistic", "hello", "hells", "hellscape"))
    }

    it should "offer list of suggestions for a prefix which matches multiple words including an exact prefix match" in {
      testTrie.suggestions("hells").sorted should be(List("hells", "hellscape"))
    }

    it should "offer list of suggestions for a prefix which matches a single word" in {
      testTrie.suggestions("wor") should be(List("world"))
    }

    it should "offer list of suggestions for a prefix which matches a whole word exactly" in {
      testTrie.suggestions("hello") should be(List("hello"))
    }

    it should "offer empty list of suggestions for a prefix which matches no words" in {
      testTrie.suggestions("Helloooo") should be(List())
    }

    it should "not alter behaviour when a duplicate word is added" in {
      val trie = testTrie
      trie.add("hello")
      trie.suggestions("hell").sorted should be(List("hellenistic", "hello", "hells", "hellscape"))
    }

  }

}

class TrieWithChildListSpec extends AnyFlatSpec with should.Matchers with SharedTrieBehaviourTests {

  "Trie using ChildListTrieNode" should behave like defaultTrie(ChildListTrie())

}

class TrieWithChildArraySpec extends AnyFlatSpec with should.Matchers with SharedTrieBehaviourTests {

  "Trie using ChildListArrayNode" should behave like defaultTrie(ChildArrayTrie())

}
