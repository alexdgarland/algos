package crackingthecodinginterview.treesandgraphs

package object model {

  case class BinaryTreeNode[T]
  (
    var value: T,
    var leftChild: Option[BinaryTreeNode[T]],
    var rightChild: Option[BinaryTreeNode[T]]
  )

  // This can be used to implement any arbitrary graph, including n-ary trees
  case class GenericNode[T](value: T, children: List[T])

  case class Graph[T](nodes: List[GenericNode[T]])

}
