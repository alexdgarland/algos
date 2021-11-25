package crackingthecodinginterview.treesandgraphs.binarytree

import scala.collection.mutable.ArrayBuffer

case class BinaryTreeNode[T]
(
  value: T,
  left: Option[BinaryTreeNode[T]] = None,
  right: Option[BinaryTreeNode[T]] = None
) {

  def visitPreOrder(visitor: T => Unit): Unit = {
    visitor(value)
    left.foreach(_.visitPreOrder(visitor))
    right.foreach(_.visitPreOrder(visitor))
  }

  def visitInOrder(visitor: T => Unit): Unit = {
    left.foreach(_.visitInOrder(visitor))
    visitor(value)
    right.foreach(_.visitInOrder(visitor))
  }

  def visitPostOrder(visitor: T => Unit): Unit = {
    left.foreach(_.visitPostOrder(visitor))
    right.foreach(_.visitPostOrder(visitor))
    visitor(value)
  }

}

case class BinaryTree[T](private val headOption: Option[BinaryTreeNode[T]] = None) {

  private val identity: T => T = value => value

  private def mapToList[R]
  (
    orderMethod: BinaryTreeNode[T] => (T => Unit) => Unit,
    f: T => R
  ): List[R] = {
    val buffer = ArrayBuffer[R]()
    headOption.foreach { orderMethod(_) { value => buffer.append(f(value)) } }
    buffer.toList
  }

  def toListPreOrder: List[T] = mapToListPreOrder(identity)

  def mapToListPreOrder[R](f: T => R): List[R] = mapToList(_.visitPreOrder, f)

  def toListInOrder: List[T] = mapToListInOrder(identity)

  def mapToListInOrder[R](f: T => R): List[R] = mapToList(_.visitInOrder, f)

  def toListPostOrder: List[T] = mapToListPostOrder(identity)

  def mapToListPostOrder[R](f: T => R): List[R] = mapToList(_.visitPostOrder, f)

}
