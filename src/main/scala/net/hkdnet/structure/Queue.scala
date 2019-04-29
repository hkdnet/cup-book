package net.hkdnet.structure

class Queue(val list: List[Int]) {
  def head: Int = list.head
  def tail: Queue = new Queue(list.tail)
  def enqueue(e: Int): Queue = new Queue(e::list)

  override def toString: String = list.mkString("Queue<", ",", ">")
}

object Queue {
  def empty: Queue = new Queue(Nil)
}
