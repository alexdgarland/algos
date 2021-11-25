package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

class GraphNode[T]
(
  val value: T,
  val children: mutable.MutableList[GraphNode[T]] = mutable.MutableList[GraphNode[T]]()
) {

  private def depthFirstSearch
  (
    predicate: T => Boolean,
    seenNodes: mutable.HashSet[Int]
  ): Option[T] = {
    children.foreach { node =>
      if(seenNodes.contains(node.hashCode()))
        return None
      seenNodes.add(node.hashCode())
      if(predicate(node.value))
        return Some(node.value)
      val innerSearchResult = node.depthFirstSearch(predicate, seenNodes)
      if (innerSearchResult.isDefined)
        return innerSearchResult
    }
    None
  }

  def depthFirstSearch(predicate: T => Boolean): Option[T] = depthFirstSearch(predicate, mutable.HashSet[Int]())

  def breadthFirstSearch(predicate: T => Boolean): Option[T] = None

}

//case class Graph[T](nodes: List[GraphNode[T]])
//
//object DirectedGraph {
//
//  def existsRouteBetween[T](node1: GraphNode[T], node2: GraphNode[T]): Boolean = {
//    false
//  }
//
//}
