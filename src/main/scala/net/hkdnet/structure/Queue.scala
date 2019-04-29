package net.hkdnet.structure

class Queue[T](val list: List[T]) {
  def head: T = list.head
  def tail: Queue[T] = new Queue(list.tail)
  def enqueue(e: T): Queue[T] = new Queue[T](e::list)

  override def toString: String = list.mkString("Queue<", ",", ">")
}
