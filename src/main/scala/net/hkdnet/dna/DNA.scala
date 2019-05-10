package net.hkdnet.dna

abstract class Base

case object A extends Base
case object T extends Base
case object G extends Base
case object U extends Base

object Base {
  val fromInt: Int => Base = Array(A, T, G, U)
  val toInt: Base => Int = Map(A -> 0, T -> 1, G -> 2, U -> 3)
}

final class DNA private(val groups: Array[Int],
                        val length: Int) extends IndexedSeq[Base] {
  import DNA._

  def apply(idx: Int): Base = {
    if (idx < 0 || length <= idx)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(idx / N) >> (idx % N * S) & M)
  }
}

object DNA {
  /***
    * S is the bit count, a DNA reqiures to be reresented.
    */
  private val S = 2
  /***
    * N is the count, a integer can contain DNA.
    */
  private val N = 32 / S
  /***
    * M is a bit mask.
    * 0b11
    */
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): DNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)

    for (i <- 0 until buf.length)
      groups(i/N) |= Base.toInt(buf(i)) << (i%N * S)

    new DNA(groups, buf.length)
  }

  def apply(bases: Base*) = fromSeq(bases)
}

