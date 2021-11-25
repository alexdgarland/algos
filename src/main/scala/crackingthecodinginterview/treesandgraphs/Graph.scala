package crackingthecodinginterview.treesandgraphs

class GraphNode[T]
(
  val value: T,
  val children: List[GraphNode[T]] = List()
) {

  // TODO - cycle detection - don't have a unit test for this yet
  def depthFirstSearch(predicate: T => Boolean): Option[T] = {
    children.foreach { node =>
      if(predicate(node.value)) return Some(node.value)
      val innerSearchResult = node.depthFirstSearch(predicate)
      if (innerSearchResult.isDefined) return innerSearchResult
    }
    None
  }

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
