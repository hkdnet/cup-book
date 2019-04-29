package net.hkdnet.structure

class Queue[T] private (private val leading: List[T], private val trailing: List[T]) {
  def this(list: List[T]) {
    this(list.take(list.size / 2), list.drop(list.size / 2).reverse)
  }

  // TODO: rebalance...?
  private def mirror: Queue[T] = {
    if (leading.isEmpty) {
      new Queue(trailing.reverse, Nil)
    } else {
      this
    }
  }

  def head: T = mirror.leading.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue(q.leading.tail, q.trailing)
  }
  def enqueue(e: T): Queue[T] = new Queue[T](leading, e::trailing)

  override def toString: String = {
    "Queue<" + leading.mkString(",") + trailing.reverse.mkString(",") + ">"
  }
}

object Queue {
  def apply[T](xs: T*) = new Queue[T](xs.toList, Nil)
}
