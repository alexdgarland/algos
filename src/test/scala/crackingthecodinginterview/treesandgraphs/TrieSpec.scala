package crackingthecodinginterview.treesandgraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait SharedTrieBehaviourTests {
  this: AnyFlatSpec with should.Matchers =>

  def defaultTrie(newTrie: => Trie): Unit = {

    def testTrie = {
      val trie = newTrie
      trie.add("Hello")
      trie.add("world")
      trie
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

  }

}

class TrieWithChildListSpec extends AnyFlatSpec with should.Matchers with SharedTrieBehaviourTests {

  "Trie using ChildListTrieNode" should behave like defaultTrie(ChildListTrie())

}

class TrieWithChildArraySpec extends AnyFlatSpec with should.Matchers with SharedTrieBehaviourTests {

  "Trie using ChildListArrayNode" should behave like defaultTrie(ChildArrayTrie())

}
