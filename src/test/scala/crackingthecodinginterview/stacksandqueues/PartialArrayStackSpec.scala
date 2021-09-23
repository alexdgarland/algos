package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PartialArrayStackSpec extends AnyFlatSpec with should.Matchers {

  private def emptyStack = TripleStack[Int]().stack1

  // This helper function also implicitly tests the default behaviour of the push method
  private def populatedStack = {
    val stack = emptyStack
    (1 to 3).foreach { stack.push }
    stack
  }

  "toList" should "return empty lists for empty stacks" in {
    emptyStack.toList should be(List())
  }

  it should "return expected list when values are populated" in {
    populatedStack.toList should be(List(3, 2, 1))
  }

  "push" should "throw an exception when stack allocation is exceeded" in {
    val stack = TripleStack[Int](3).stack1
    (1 to 3).foreach { stack.push }
    assertThrows[IllegalStateException] {
      stack.push(4)
    }
  }

  "pop" should "return None for empty stack" in {
    emptyStack.pop() should be(None)
  }

  it should "remove and return value as expected from populated stack" in {
    val stack = populatedStack
    stack.pop() should be(Some(3))
    stack.toList should be(List(2, 1))
  }

  "peek" should "return None for empty stack" in {
    emptyStack.peek() should be(None)
  }

  it should "return value without removing from populated stack" in {
    val stack = populatedStack
    stack.peek() should be(Some(3))
    stack.toList should be(List(3, 2, 1))
  }

  "isEmpty" should "return true for empty stack" in {
    emptyStack.isEmpty should be(true)
  }

  it should "return false for populated stack" in {
    populatedStack.isEmpty should be(false)
  }

}
