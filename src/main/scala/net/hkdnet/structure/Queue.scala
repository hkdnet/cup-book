package net.hkdnet.structure

class Queue[T](private val r: List[T], private val s: List[T]) {
  def this(list: List[T]) {
    this(list.take(list.size / 2), list.drop(list.size / 2).reverse)
  }
  def head: T = r.head
  def tail: Queue[T] = new Queue(r.tail, s)
  def enqueue(e: T): Queue[T] = new Queue[T](r, e::s)
  // TODO: rebalance...

  override def toString: String = {
    "Queue<" + r.mkString(",") + s.reverse.mkString(",") + ">"
  }
}
