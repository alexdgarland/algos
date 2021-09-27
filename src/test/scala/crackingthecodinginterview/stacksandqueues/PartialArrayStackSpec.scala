package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class PartialArrayStackSpec extends AnyFlatSpec with should.Matchers with SharedStackBehaviourTests {

  def partialArrayStack(getSelectedStack: TripleStack[Int] => PartialArrayStack[Int]): Unit = {

    def newPartialArrayStack = getSelectedStack(TripleStack[Int]())

    it should behave like defaultStack(newPartialArrayStack)

    it should "throw an exception on push when stack allocation is exceeded" in {
      val stack = getSelectedStack(TripleStack[Int](3))
      populateStack(stack)
      assertThrows[IllegalStateException] {
        stack.push(4)
      }
    }

  }

  "first of three stacks" should behave like partialArrayStack(_.stack1)

  "second of three stacks" should behave like partialArrayStack(_.stack2)

  "third of three stacks" should behave like partialArrayStack(_.stack3)

}
