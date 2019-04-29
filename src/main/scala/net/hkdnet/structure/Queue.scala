package net.hkdnet.structure

trait Queue[T] {
  def head: T
  def tail: Queue[T]
  def enqueue(e: T): Queue[T]
}


object Queue {
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList)

  private class QueueImpl[T] private (private val leading: List[T], private val trailing: List[T]) extends Queue[T] {
    def this(list: List[T]) {
      this(list.take(list.size / 2), list.drop(list.size / 2).reverse)
    }

    // TODO: rebalance...?
    private def mirror: QueueImpl[T] = {
      if (leading.isEmpty) {
        new QueueImpl(trailing.reverse, Nil)
      } else {
        this
      }
    }

    def head: T = mirror.leading.head
    def tail: Queue[T] = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }
    def enqueue(e: T): Queue[T] = new QueueImpl[T](leading, e::trailing)

    override def toString: String = {
      "Queue<" + leading.mkString(",") + trailing.reverse.mkString(",") + ">"
    }
  }
}
