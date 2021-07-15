package crackingthecodinginterview.arraysandstrings

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should
import Urlify._

class UrlifySpec extends AnyFlatSpec with should.Matchers {

  "urlify function" should "convert spaces to %20" in {
    val array = "Mr John Smith    ".toCharArray
    urlify(array, 13)
    array.mkString should be("Mr%20John%20Smith")

    "aaaa".toCharArray.length
  }

}
