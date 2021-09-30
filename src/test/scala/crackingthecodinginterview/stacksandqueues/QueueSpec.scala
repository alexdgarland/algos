package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class QueueSpec extends AnyFlatSpec with should.Matchers {

  "Queue" should "return received values as expected when all removes done sequentially" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.add(3)
    queue.remove() should be(Some(1))
    queue.remove() should be(Some(2))
    queue.remove() should be(Some(3))
    queue.remove() should be(None)
  }

  it should "return received values as expected when removes interpolated with adds" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.remove() should be(Some(1))
    queue.add(3)
    queue.remove() should be(Some(2))
    queue.remove() should be(Some(3))
    queue.remove() should be(None)
  }

  it should "return values without removing when peek is called" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.peek() should be(Some(1))
    queue.peek() should be(Some(1)) // Second call to make sure value not removed
  }

  it should "report correct size for initial empty queue" in {
    Queue[Int]().size should be(0)
  }

  it should "report correct size for populated queue" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.add(3)
    queue.size should be(3)
  }

  it should "report correct size for queue with some elements removed" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.add(3)
    queue.remove()
    queue.size should be(2)
  }

  it should "report correct size for re-emptied queue" in {
    val queue = Queue[Int]()
    queue.add(1)
    queue.add(2)
    queue.add(3)
    (1 to 3).foreach(_ => queue.remove())
    queue.size should be(0)
  }

}
