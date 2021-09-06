package crackingthecodinginterview.linkedlists

/**
 *  Class containing the logic to sum two lists of integers using an arbitrary builder class to produce the final list.
 *
 * @tparam N Type of nodes to handle
 * @tparam L Type of lists to handle
 * @tparam B Type of builder to use
 */
private trait IntListSummation[N <: ListNode[Int, N], L <: LinkedList[Int, N, _], B] {

  private def getValue(nodeOption: Option[N]): Int = nodeOption.map(_.value).getOrElse(0)

  def newBuilder(): B

  def addValue(builder: B, value: Int): Unit

  def build(builder: B): L

  /**
   * Take two lists of ints, representing a number with each node as a single digit in reverse order,
   * and add them together maintaining the same representation in an efficient way (O(n)).
   *
   * @param list1 First list to add
   * @param list2 Second list to add
   * @return
   */
  def sumLists(list1: L, list2: L): L = {
    val builder = newBuilder()
    var list1Node = list1.head
    var list2Node = list2.head
    var carry = false

    while(list1Node.isDefined || list2Node.isDefined) {
      val total = getValue(list1Node) + getValue(list2Node) + (if(carry) 1 else 0)
      carry = total > 9
      addValue(builder, total % 10)
      list1Node = list1Node.flatMap(_.next)
      list2Node = list2Node.flatMap(_.next)
    }

    if(carry) addValue(builder, 1)

    build(builder)
  }

}
