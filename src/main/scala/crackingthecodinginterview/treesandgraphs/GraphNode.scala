package crackingthecodinginterview.treesandgraphs

import scala.collection.mutable

private case class NodeStack[T](private var list: List[GraphNode[T]] = List[GraphNode[T]]()) {

  def push(node: GraphNode[T]): Unit = list = node :: list

  def pop(): GraphNode[T] = {
    val node::newList = list
    list = newList
    node
  }

  def nonEmpty: Boolean = list.nonEmpty

}

private case class SeenNodeTracker(private val set: mutable.HashSet[Int] = mutable.HashSet[Int]()) {
  def addSeen(node: GraphNode[_]): Unit = set.add(node.hashCode())

  def hasBeenSeen(node: GraphNode[_]): Boolean = set.contains(node.hashCode())
}

class GraphNode[T]
(
  val value: T,
  val children: mutable.MutableList[GraphNode[T]] = mutable.MutableList[GraphNode[T]]()
) {

  override def toString: String = s"${super.toString} - value $value"

  def depthFirstSearch(predicate: T => Boolean): Option[T] = {
    val stack = NodeStack[T]()
    stack.push(this)
    val seenNodes = SeenNodeTracker()
    while(stack.nonEmpty) {
      val currentNode = stack.pop()
      if(!seenNodes.hasBeenSeen(currentNode)) {
        seenNodes.addSeen(currentNode)
        if (predicate(currentNode.value))
          return Some(currentNode.value)
        currentNode
          .children
          .reverse
          .foreach { stack.push }
      }
    }
    None
  }

  def breadthFirstSearch(predicate: T => Boolean): Option[T] = {
    val queue = mutable.Queue[GraphNode[T]]()
    queue.enqueue(this)
    val seenNodes = SeenNodeTracker()
    while(queue.nonEmpty) {
      val currentNode = queue.dequeue()
      if(!seenNodes.hasBeenSeen(currentNode)) {
        seenNodes.addSeen(currentNode)
        if(predicate(currentNode.value))
          return Some(currentNode.value)
        currentNode
          .children
          // Filter is slightly redundant functionally, but reduces queue space usage
          .filter { childNode => !seenNodes.hasBeenSeen(childNode) }
          .foreach { queue.enqueue(_) }
      }
    }
    None
  }

}
