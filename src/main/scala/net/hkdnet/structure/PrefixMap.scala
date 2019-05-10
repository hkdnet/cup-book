package net.hkdnet.structure

import scala.collection.{immutable, mutable}

class PrefixMap[T] extends mutable.Map[String, T] with mutable.MapLike[String, T, PrefixMap[T]] {

  var suffixes: immutable.Map[Char, PrefixMap[T]] = Map.empty
  var value: Option[T] = None

  def get(s: String): Option[T] =
    if (s.isEmpty) value
    else suffixes.get(s(0)) flatMap (_.get(s substring (1)))

  def withPrefix(s: String): PrefixMap[T] =
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes get leading match {
        case None => suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading) withPrefix (s substring 1)
    }

  override def update(s: String, elem: T) = withPrefix(s).value = Some(elem)

  override def remove(s: String): Option[T] =
    if (s.isEmpty) {
      val prev = value
      value = None
      prev
    } else {
      suffixes get (s(0)) flatMap (_.remove(s substring 1))
    }

  def iterator: Iterator[(String, T)] =
    (for (v <- value.iterator) yield ("", v)) ++
      (for ((chr, m) <- suffixes.iterator;
            (s, v) <- m.iterator) yield (chr +: s, v))

  def +=(kv: (String, T)): this.type = {
    update(kv._1, kv._2)
    this
  }

  def -=(key: String): this.type = {
    remove(key)
    this
  }

  override def empty = new PrefixMap[T]
}

object PrefixMap {
  def empty[T] = new PrefixMap[T]

  def apply[T](kvs: (String, T)*) = {
    val e = empty[T]
    for (kv <- kvs) e += kv
    e
  }
}
