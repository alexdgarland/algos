package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class SetOfStacksSpec extends AnyFlatSpec with should.Matchers with SharedStackBehaviourTests {

  private def newFiniteStackWithCapacity4 = new FiniteStack[Int](4)

  "FiniteStack" should behave like defaultStack(newFiniteStackWithCapacity4)

  it should "not be marked as full when first created" in {
    new FiniteStack[Int](3).isFull should be(false)
  }

  it should "not be marked as full when a single value is added" in {
    val stack = new FiniteStack[Int](3)
    stack.push(100)
    stack.isFull should be(false)
  }

  it should "be marked as full when capacity is reached" in {
    val stack = new FiniteStack[Int](3)
    populateStack(stack)
    stack.isFull should be(true)
  }

  it should "throw an exception when capacity is exceeded" in {
    val stack = new FiniteStack[Int](3)
    populateStack(stack)
    assertThrows[IllegalStateException] {
      stack.push(4)
    }
  }

  it should "not throw an exception upon push when an element is popped first to keep stack under capacity" in {
    val stack = new FiniteStack[Int](3)
    populateStack(stack)
    stack.pop()
    stack.push(4)
    stack.toList() should be(List(4, 2, 1))
  }

  private def newSetOfStacksWithInnerCapacity2 = new SetOfStacks[Int](2)

  "SetOfStacks" should behave like defaultStack(newSetOfStacksWithInnerCapacity2)

  it should "drain as expected as sequential pops are executed" in {
    val stack = new SetOfStacks[Int](2)

    populateStack(stack)

    stack.pop() should be(Some(3))
    stack.isEmpty should be(false)

    stack.pop() should be(Some(2))
    stack.isEmpty should be(false)

    stack.pop() should be(Some(1))
    stack.isEmpty should be(true)
    stack.toList should be(List())
    stack.peek() should be(None)
    stack.pop() should be(None)
  }

}
