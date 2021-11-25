package crackingthecodinginterview.treesandgraphs

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.collection.mutable

class GraphSpec extends AnyFlatSpec with should.Matchers {

  private case class ValueStructure(description: String, property1: Int)

  private val nodeWithNoChildren = new GraphNode(ValueStructure("Only node", 1))

  "GraphNode without children" should "be able to perform depth-first search" in {
    nodeWithNoChildren.depthFirstSearch(_.property1 > 2) should be(None)
  }

  it should "be able to perform breadth-first search" in {
    nodeWithNoChildren.breadthFirstSearch(_.property1 > 2) should be(None)
  }

  private val nonMatchingValue = ValueStructure("Doesn't meet predicate", 2)

  private val depthFirstNodeDescription = "This will be found first in depth-first-search"

  private val breadthFirstNodeDescription = "This will be found first in breadth-first-search"

  private val nodeWithChildren = new GraphNode(
    ValueStructure("Starting node", 1),
    mutable.MutableList(
      new GraphNode(
        nonMatchingValue,
        mutable.MutableList(
          new GraphNode(nonMatchingValue),
          new GraphNode(ValueStructure(depthFirstNodeDescription, 3))
        )
      ),
      new GraphNode(nonMatchingValue),
      new GraphNode(ValueStructure(breadthFirstNodeDescription, 3))
    )
  )

  "GraphNode with children" should "be able to perform depth-first search" in {
    nodeWithChildren
      .depthFirstSearch(_.property1 > 2)
      .map(_.description) should be(Some(depthFirstNodeDescription))
  }

  it should "be able to perform breadth-first search" in {
    nodeWithChildren
      .breadthFirstSearch(_.property1 > 2)
      .map(_.description) should be(Some(breadthFirstNodeDescription))
  }

  private val nodeWithCircularReference = {
    val node1 = new GraphNode(ValueStructure("Starting node", 1))
    val node2 = new GraphNode(nonMatchingValue)
    val node3 = new GraphNode(nonMatchingValue)
    node1.children += node2
    node2.children += node3
    node3.children += node1
    node1
  }

  "GraphNode with circular reference" should "be able to perform depth-first search" in {
    nodeWithCircularReference
      .depthFirstSearch(_.property1 > 2)
      .map(_.description) should be(None)
  }

  //  it should "be able to perform breadth-first search" in {
  //    nodeWithChildren
  //      .breadthFirstSearch(_.property1 > 2)
  //      .map(_.description) should be(None)
  //  }

}
