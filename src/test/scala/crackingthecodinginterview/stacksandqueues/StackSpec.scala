package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class StackSpec extends AnyFlatSpec with should.Matchers with SharedStackBehaviourTests {

  private def newDefaultStack = Stack[Int]()

  "default stack implementation" should behave like defaultStack(newDefaultStack)

}
