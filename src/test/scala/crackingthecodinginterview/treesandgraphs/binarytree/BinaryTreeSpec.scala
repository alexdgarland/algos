package crackingthecodinginterview.treesandgraphs.binarytree

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

class BinaryTreeSpec extends AnyFlatSpec with should.Matchers {

  private def shouldReturnEmptyListForEmptyTree(method: BinaryTree[Int] => List[Int]): Unit = {
    method(BinaryTree[Int]()) should be(List())
  }

  "Empty BinaryTree" should "support pre-order list creation" in {
    shouldReturnEmptyListForEmptyTree(_.toListPreOrder)
  }

  it should "support pre-order mapping" in {
    shouldReturnEmptyListForEmptyTree(_.mapToListPreOrder(_ * 2))
  }

  it should "support in-order list creation" in {
    shouldReturnEmptyListForEmptyTree(_.toListInOrder)
  }

  it should "support in-order mapping" in {
    shouldReturnEmptyListForEmptyTree(_.mapToListInOrder(_ * 2))
  }

  it should "support post-order list creation" in {
    shouldReturnEmptyListForEmptyTree(_.toListPostOrder)
  }

  it should "support post-order mapping" in {
    shouldReturnEmptyListForEmptyTree(_.mapToListPostOrder(_ * 2))
  }

  /**
   *                1
   *        2               3
   *    4       5       6       7
   */
  private val testTree = BinaryTree[Int](
    Some(BinaryTreeNode(
      1,
      Some(BinaryTreeNode(
        2,
        Some(BinaryTreeNode(4)),
        Some(BinaryTreeNode(5))
      )),
      Some(BinaryTreeNode(
        3,
        Some(BinaryTreeNode(6)),
        Some(BinaryTreeNode(7))
      ))
    ))
  )

  "Populated BinaryTree" should "support pre-order list creation" in {
    testTree.toListPreOrder should be(List(1, 2, 4, 5, 3, 6, 7))
  }

  it should "support pre-order mapping" in {
    testTree.mapToListPreOrder(_ * 2) should be(List(2, 4, 8, 10, 6, 12, 14))
  }

  it should "support in-order list creation" in {
    testTree.toListInOrder should be(List(4, 2, 5, 1, 6, 3, 7))
  }

  it should "support in-order mapping" in {
    testTree.mapToListInOrder(_ * 2) should be(List(8, 4, 10, 2, 12, 6, 14))
  }

  it should "support post-order list creation" in {
    testTree.toListPostOrder should be(List(4, 5, 2, 6, 7, 3, 1))
  }

  it should "support post-order mapping" in {
    testTree.mapToListPostOrder(_ * 2) should be(List(8, 10, 4, 12, 14, 6, 2))
  }

}
