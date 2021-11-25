package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

case class GraphNode[T]
(
  value: T,
  children: mutable.MutableList[GraphNode[T]] = mutable.MutableList[GraphNode[T]]()
) {

  def depthFirstSearch(predicate: T => Boolean): Option[T] = None

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
