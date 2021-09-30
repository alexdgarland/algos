package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait SharedStackBehaviourTests {
  this: AnyFlatSpec with should.Matchers =>

  protected def populateStack(stack: Stack[Int]): Unit = (1 to 3).foreach { stack.push }

  def defaultStack(newStack: => Stack[Int]): Unit = {

    def populatedStack: Stack[Int] = {
      val stack = newStack
      populateStack(stack)
      stack
    }

    it should "return empty list from toList for empty stack" in {
      newStack.toList should be(List[Int]())
    }

    it should "accept pushed values" in {
      populatedStack.toList should be(List(3, 2, 1))
    }

    it should "remove and return popped value" in {
      val stack = populatedStack
      stack.pop() should be(Some(3))
      stack.toList should be(List(2, 1))
    }

    it should "return None from pop on empty stack" in {
      newStack.pop() should be(None)
    }

    it should "return None from peek on empty stack" in {
      newStack.peek() should be(None)
    }

    it should "return peeked value without removing" in {
      val stack = populatedStack
      stack.peek() should be(Some(3))
      stack.toList should be(List(3, 2, 1))
    }

    it should "return true for isEmpty on empty stack" in {
      newStack.isEmpty should be(true)
    }

    it should "return false for isEmpty on populated stack" in {
      populatedStack.isEmpty should be(false)
    }

    it should "return size 0 for new empty stack" in {
      newStack.size should be(0)
    }

    it should "return correct size for populated stack" in {
      populatedStack.size should be(3)
    }

    it should "return correct size for stack with some elements popped" in {
      val stack = populatedStack
      stack.pop()
      stack.size should be(2)
    }

    it should "return correct size for re-emptied stack" in {
      val stack = populatedStack
      (1 to 3).foreach(_ => stack.pop())
      stack.size should be(0)
    }

  }

}
