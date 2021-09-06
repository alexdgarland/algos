package crackingthecodinginterview.linkedlists

private trait ListMapper[
  T, N <: ListNode[T, N], L <: LinkedList[T, N, _], TT, NN <: ListNode[TT, NN], LL <: LinkedList[TT, NN, _]
] {

  def newList(): LL

  def newNode(value: TT, previousNode: Option[NN] = None): NN

  def map(list: L, f: T => TT): LL = {
    val mappedList: LL = newList()
    list.head match {
      case None =>
      // Do nothing further
      case Some(headNode) =>
        val mappedHeadNode = newNode(f(headNode.value))
        mappedList.head = Some(mappedHeadNode)
        var currentSourceNode = headNode.next
        var latestAttachedMapNode = mappedHeadNode
        while(currentSourceNode.isDefined) {
          val nextMappedNode = newNode(f(currentSourceNode.get.value), Some(latestAttachedMapNode))
          latestAttachedMapNode.next = Some(nextMappedNode)
          latestAttachedMapNode = nextMappedNode
          currentSourceNode = currentSourceNode.get.next
        }
        mappedList.setToTail(Some(latestAttachedMapNode))
    }
    mappedList
  }

}
