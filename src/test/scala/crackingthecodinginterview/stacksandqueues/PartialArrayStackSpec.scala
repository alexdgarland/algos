package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PartialArrayStackSpec extends AnyFlatSpec with should.Matchers {

  "toList" should "return empty lists for empty stacks" in {
    val emptyStacks = TripleStack[Int]()
    emptyStacks.stack1.toList should be(List())
    emptyStacks.stack2.toList should be(List())
    emptyStacks.stack3.toList should be(List())
  }

  it should "return expected list when values are populated" in {
    val stack = TripleStack[Int]().stack1
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack.toList should be(List(3, 2, 1))
  }

}
