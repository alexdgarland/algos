package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StackSpec extends AnyFlatSpec with should.Matchers {

  "push" should "add item to stack as expected" in {
    val stack = Stack(3, 2, 1)
    stack.push(4)
    stack.toList should be(List(4, 3, 2, 1))
  }

  "push" should "remove item from populated stack and return it wrapped in Some as expected" in {
    val stack = Stack(3, 2, 1)
    stack.pop() should be(Some(3))
    stack.toList should be(List(2, 1))
  }

  it should "return None when stack is empty" in {
    val stack = Stack[Int]()
    stack.pop() shouldBe None
  }

  "peek" should "return item from populated stack wrapped in Some without altering stack" in {
    val stack = Stack(3, 2, 1)
    stack.peek() should be(Some(3))
    stack.toList should be(List(3, 2, 1))
  }

  it should "return None when stack is empty" in {
    val stack = Stack[Int]()
    stack.peek() shouldBe None
  }

  "isEmpty" should "return true when stack is empty" in {
    Stack[Int]().isEmpty should be(true)
  }

  it should "return false when stack is populated" in {
    Stack(1, 2, 3).isEmpty should be(false)
  }

}
