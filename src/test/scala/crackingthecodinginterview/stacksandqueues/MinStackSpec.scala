package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class MinStackSpec extends AnyFlatSpec with should.Matchers {

  // TODO - basic stack operation tests are repeated in several places;
  //  could refactor to shared test code for the different impls once am not in the thick of writing new impl code

  private def populatedStack() = {
    val stack = MinStack[Int]()
    stack.push(1)
    stack.push(2)
    stack.push(3)
    stack
  }

  "MinStack" should "return empty list from toList for empty stack" in {
    MinStack[Int]().toList should be(List[Int]())
  }

  it should "accept pushed values" in {
    populatedStack().toList should be(List(3, 2, 1))
  }

  it should "remove and return popped value" in {
    val stack = populatedStack()
    stack.pop() should be(Some(3))
    stack.toList should be(List(2, 1))
  }

  it should "return None from pop on empty stack" in {
    MinStack[Int]().pop() should be(None)
  }

  it should "return None from peek on empty stack" in {
    MinStack[Int]().peek() should be(None)
  }

  it should "return peeked value without removing" in {
    val stack = populatedStack()
    stack.peek() should be(Some(3))
    stack.toList should be(List(3, 2, 1))
  }

  it should "return true for isEmpty on empty stack" in {
    MinStack[Int]().isEmpty should be(true)
  }

  it should "return false for isEmpty on populated stack" in {
    populatedStack().isEmpty should be(false)
  }

  it should "return min of None for empty stack" in {
    MinStack[Int]().min should be(None)
  }

  it should "return correct min once stack is populated" in {
    populatedStack().min should be(Some(1))
  }

  it should "continue to track correct min once values are popped from populated stack" in {
    val stack = MinStack[Int]()
    stack.push(3)
    stack.push(2)
    stack.push(1)
    stack.pop()
    stack.min should be(Some(2))
  }

  it should "not update min if popped value was not existing min" in {
    val stack = MinStack[Int]()
    stack.push(2)
    stack.push(1)
    stack.push(3)
    stack.pop()
    stack.min should be(Some(1))
  }

  it should "continue to track correct min once values are popped from populated stack with duplicate values" in {
    val stack = MinStack[Int]()
    stack.push(2)
    stack.push(1)
    stack.push(1)
    stack.pop()
    stack.min should be(Some(1))
  }

}
