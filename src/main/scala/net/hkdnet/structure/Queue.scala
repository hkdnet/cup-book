package net.hkdnet.structure

trait Queue[+T] {
  def head: T

  def tail: Queue[T]

  def enqueue[U >: T](e: U): Queue[U]
}


object Queue {
  def apply[T](xs: T*): Queue[T] = new QueueImpl[T](xs.toList)

  private class QueueImpl[+T] private(private[this] var leading: List[T],
                                      private[this] var trailing: List[T]) extends Queue[T] {
    def this(list: List[T]) {
      this(list.take(list.size / 2), list.drop(list.size / 2).reverse)
    }

    private def mirror() = {
      if (leading.isEmpty) {
        while (!trailing.isEmpty) {
          leading = trailing.head :: leading
          trailing = trailing.tail
        }
      }
    }

    def head: T = {
      leading.head
    }

    def tail: Queue[T] = {
      mirror()
      new QueueImpl(leading.tail, trailing)
    }

    def enqueue[U >: T](e: U): Queue[U] = new QueueImpl[U](leading, e :: trailing)

    override def toString: String = {
      "Queue<" + leading.mkString(",") + trailing.reverse.mkString(",") + ">"
    }
  }

}
