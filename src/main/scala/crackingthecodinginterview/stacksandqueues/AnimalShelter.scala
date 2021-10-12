package crackingthecodinginterview.stacksandqueues

sealed trait Animal {
  val name: String
}

case class Cat(name: String) extends Animal

case class Dog(name: String) extends Animal

/**
 * Class to represent an animal shelter,
 * which holds only dogs and cats and operates on a strictly "first in, first out" basis.
 *
 * People must adopt either the "oldest" (based on arrival time) of all animals at the shelter,
 * or they can select whether they would prefer a dog or a cat (and will receive the oldest animal of that type).
 *
 * They cannot select which specific animal they would like.
 *
 * Internally the implementation uses queues, but method names are aligned to the domain not the implementation.
 *
 */
class AnimalShelter() {

  private var latestSequenceNumber: Int = 0

  /**
   * We're going to allocate a sequence number here to track which animal is the oldest across the two queues.
   *
   * We could alternatively use timestamps;
   * the sequence is fully deterministic (never ties) and hence slightly easier to unit-test.
   *
   * Technically it creates a bottle-neck were we to scale it aggressively, but that's beyond immediate scope,
   * and at true (distributed) scale one also runs into CAP issues (clock consistency).
   */
  private case class SequentialSlot[A <: Animal]
  (
    animal: A,
    sequenceNumber: Int = {
      latestSequenceNumber += 1
      latestSequenceNumber
    }
  )

  private case class AnimalQueue[A <: Animal]
  (
    private val slotQueue: Queue[SequentialSlot[A]] = new LinkedListQueue[SequentialSlot[A]]
  ) {
    def earliestSequence: Int = slotQueue.peek().map(_.sequenceNumber).getOrElse(Int.MaxValue)

    def oldestAnimal: Option[A] = slotQueue.dequeue().map(_.animal)

    def enqueue(animal: A): Unit = slotQueue.enqueue(SequentialSlot(animal))
  }

  private val catQueue = AnimalQueue[Cat]()
  private val dogQueue = AnimalQueue[Dog]()

  def rescue(animal: Animal): Unit = animal match {
    case cat@Cat(_) => catQueue.enqueue(cat)
    case dog@Dog(_) => dogQueue.enqueue(dog)
  }

  def adoptAnimal: Option[Animal] = if (catQueue.earliestSequence < dogQueue.earliestSequence) adoptCat else adoptDog

  def adoptCat: Option[Cat] = catQueue.oldestAnimal

  def adoptDog: Option[Dog] = dogQueue.oldestAnimal

}
