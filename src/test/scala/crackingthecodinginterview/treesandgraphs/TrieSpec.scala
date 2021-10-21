package crackingthecodinginterview.treesandgraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class TrieSpec extends AnyFlatSpec with should.Matchers {

  private def testTrie = {
    val trie = new Trie()
    trie.add("Hello")
    trie.add("world")
    trie
  }

  "Trie" should "identify word that is contained" in {
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

}
