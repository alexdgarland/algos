package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

trait SharedQueueBehaviourTests {
  this: AnyFlatSpec with should.Matchers =>

  def defaultQueue(newQueue: => Queue[Int]): Unit = {

    it should "return received values as expected when all dequeues done sequentially" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.dequeue() should be(Some(1))
      queue.dequeue() should be(Some(2))
      queue.dequeue() should be(Some(3))
      queue.dequeue() should be(None)
    }

    it should "return received values as expected when dequeues interpolated with enqueues" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.dequeue() should be(Some(1))
      queue.enqueue(3)
      queue.dequeue() should be(Some(2))
      queue.dequeue() should be(Some(3))
      queue.dequeue() should be(None)
    }

    it should "return values without removing when peek is called" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.peek() should be(Some(1))
      queue.peek() should be(Some(1)) // Second call to make sure value not removed
    }

    it should "report correct length for initial empty queue" in {
      newQueue.length should be(0)
    }

    it should "report correct length for populated queue" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.length should be(3)
    }

    it should "report correct length for queue with some elements removed" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      queue.dequeue()
      queue.length should be(2)
    }

    it should "report correct length for re-emptied queue" in {
      val queue = newQueue
      queue.enqueue(1)
      queue.enqueue(2)
      queue.enqueue(3)
      (1 to 3).foreach(_ => queue.dequeue())
      queue.length should be(0)
    }

  }

}
