package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StackSpec extends AnyFlatSpec with should.Matchers with SharedStackBehaviourTests {

  private def newDefaultStack = Stack[Int]()

  "default stack implementation" should behave like defaultStack(newDefaultStack)

  "sort method" should "sort a stack as expected" in {
    val stack = Stack(5, 2, 7, 2, 3, 9)
    Stack.sort(stack)
    stack.toList should be(List(2, 2, 3, 5, 7, 9))
  }

}
