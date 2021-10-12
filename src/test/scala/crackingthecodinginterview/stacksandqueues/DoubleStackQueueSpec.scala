package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class DoubleStackQueueSpec extends AnyFlatSpec with should.Matchers with SharedQueueBehaviourTests {

  private def newDoubleStackQueue = DoubleStackQueue[Int]()

  "DoubleStackQueue" should behave like defaultQueue(newDoubleStackQueue)

}
