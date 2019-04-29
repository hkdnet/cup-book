package net.hkdnet.structure

class Queue[T](private val r: List[T], private val s: List[T]) {
  def this(list: List[T]) {
    this(list.take(list.size / 2), list.drop(list.size / 2).reverse)
  }

  // TODO: rebalance...?
  private def mirror: Queue[T] = {
    if (r.isEmpty) {
      new Queue(s.reverse, Nil)
    } else {
      this
    }
  }

  def head: T = mirror.r.head
  def tail: Queue[T] = {
    val q = mirror
    new Queue(q.r.tail, q.s)
  }
  def enqueue(e: T): Queue[T] = new Queue[T](r, e::s)

  override def toString: String = {
    "Queue<" + r.mkString(",") + s.reverse.mkString(",") + ">"
  }
}
