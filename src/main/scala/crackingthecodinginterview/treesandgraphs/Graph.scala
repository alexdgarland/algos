package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

class GraphNode[T]
(
  val value: T,
  val children: mutable.MutableList[GraphNode[T]] = mutable.MutableList[GraphNode[T]]()
) {

  def depthFirstSearch(predicate: T => Boolean): Option[T] = depthFirstSearch(predicate, mutable.HashSet[Int]())

  private def depthFirstSearch
  (
    predicate: T => Boolean,
    seenNodes: mutable.HashSet[Int]
  ): Option[T] = {
    // TODO this doesn't currently check whether the root node matches predicate
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

  def breadthFirstSearch(predicate: T => Boolean): Option[T] = {
    val seenNodes = mutable.HashSet[Int]()
    val queue = mutable.Queue[GraphNode[T]]()
    queue.enqueue(this)
    while(queue.nonEmpty) {
      val currentNode = queue.dequeue()
      if(!seenNodes.contains(currentNode.hashCode())) {
        seenNodes.add(currentNode.hashCode())
        if(predicate(currentNode.value))
          return Some(currentNode.value)
        currentNode.children.foreach(queue.enqueue(_))
      }
    }
    None
  }

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
