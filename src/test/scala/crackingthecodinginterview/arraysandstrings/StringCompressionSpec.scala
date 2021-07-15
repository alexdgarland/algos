package crackingthecodinginterview.arraysandstrings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import StringCompression.compressString

class StringCompressionSpec extends AnyFlatSpec with should.Matchers {

  "compressString function" should "compress string where possible" in {
    compressString("aaaaabbbcccccccddd") should be("a5b3c7d3")
  }

  it should "return original string where compression would not reduce length" in {
    compressString("abbbccdd") should be("abbbccdd")
  }

}
