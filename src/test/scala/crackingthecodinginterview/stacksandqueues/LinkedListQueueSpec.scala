package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class LinkedListQueueSpec extends AnyFlatSpec with should.Matchers with SharedQueueBehaviourTests {

  private def newLinkedListQueue = new LinkedListQueue[Int]()

  "LinkedListQueue" should behave like defaultQueue(newLinkedListQueue)

}
