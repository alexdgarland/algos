package crackingthecodinginterview.stacksandqueues

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class AnimalShelterSpec extends AnyFlatSpec with should.Matchers {

  "Empty AnimalShelter" should "return None when asked for a cat" in {
    new AnimalShelter().adoptCat should be(None)
  }

  it should "return None when asked for a dog" in {
    new AnimalShelter().adoptDog should be(None)
  }

  it should "return None when asked for any animal" in {
    new AnimalShelter().adoptAnimal should be(None)
  }

  private def catOnlyShelter = {
    val shelter = new AnimalShelter()
    shelter.rescue(Cat("Archie"))
    shelter.rescue(Cat("Betty"))
    shelter.rescue(Cat("Clarence"))
    shelter
  }

  "Shelter with only cats" should "return oldest cat when asked for a cat" in {
    val shelter = catOnlyShelter
    shelter.adoptCat should be(Some(Cat("Archie")))
    shelter.adoptCat should be(Some(Cat("Betty")))
  }

  it should "return None when asked for a dog" in {
    catOnlyShelter.adoptDog should be(None)
  }

  it should "return oldest cat when asked for any animal" in {
    val shelter = catOnlyShelter
    shelter.adoptAnimal should be(Some(Cat("Archie")))
    shelter.adoptAnimal should be(Some(Cat("Betty")))
  }

  private def dogOnlyShelter = {
    val shelter = new AnimalShelter()
    shelter.rescue(Dog("Dobie"))
    shelter.rescue(Dog("Eckles"))
    shelter.rescue(Dog("Floof"))
    shelter
  }

  "Shelter with only dogs" should "return oldest cat when asked for a cat" in {
    val shelter = dogOnlyShelter
    shelter.adoptDog should be(Some(Dog("Dobie")))
    shelter.adoptDog should be(Some(Dog("Eckles")))
  }

  it should "return None when asked for a cat" in {
    dogOnlyShelter.adoptCat should be(None)
  }

  it should "return oldest dog when asked for any animal" in {
    val shelter = dogOnlyShelter
    shelter.adoptAnimal should be(Some(Dog("Dobie")))
    shelter.adoptAnimal should be(Some(Dog("Eckles")))
  }

  private def mixedShelter = {
    val shelter = new AnimalShelter()
    shelter.rescue(Cat("Archie"))
    shelter.rescue(Dog("Dobie"))
    shelter.rescue(Cat("Betty"))
    shelter.rescue(Dog("Eckles"))
    shelter.rescue(Cat("Clarence"))
    shelter.rescue(Dog("Floof"))
    shelter
  }

  "Shelter with both dogs and cats" should "return oldest cat when asked for a cat" in {
    val shelter = mixedShelter
    shelter.adoptCat should be(Some(Cat("Archie")))
    shelter.adoptCat should be(Some(Cat("Betty")))
  }

  it should "return oldest dog when asked for a dog" in {
    val shelter = mixedShelter
    shelter.adoptDog should be(Some(Dog("Dobie")))
    shelter.adoptDog should be(Some(Dog("Eckles")))
  }

  it should "return oldest animal regardless of species" in {
    val shelter = mixedShelter
    shelter.adoptAnimal should be(Some(Cat("Archie")))
    shelter.adoptAnimal should be(Some(Dog("Dobie")))
  }

}
