package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MinStackSpec extends AnyFlatSpec with should.Matchers with SharedStackBehaviourTests {

  private def newMinStack = MinStack[Int]()

  "MinStack" should behave like defaultStack(newMinStack)

  it should "return min of None for empty stack" in {
    MinStack[Int]().min should be(None)
  }

  it should "return correct min once stack is populated" in {
    val stack = newMinStack
    populateStack(stack)
    stack.min should be(Some(1))
  }

  it should "continue to track correct min once values are popped from populated stack" in {
    val stack = newMinStack
    stack.push(3)
    stack.push(2)
    stack.push(1)
    stack.pop()
    stack.min should be(Some(2))
  }

  it should "not update min if popped value was not existing min" in {
    val stack = newMinStack
    stack.push(2)
    stack.push(1)
    stack.push(3)
    stack.pop()
    stack.min should be(Some(1))
  }

  it should "continue to track correct min once values are popped from populated stack with duplicate values" in {
    val stack = newMinStack
    stack.push(2)
    stack.push(1)
    stack.push(1)
    stack.pop()
    stack.min should be(Some(1))
  }

}
