package net.hkdnet.layout

import org.scalatest._

class ElementSpec extends FlatSpec with Matchers {
  "Element.elem(Array(\"aaa\", \"bbb\"))" should "equals to \"aaa\nbbb\"" in {
    val e = Element.elem(Array("aaa", "bbb"))
    e.toString shouldEqual "aaa\nbbb"
  }
  "Element.elem('c', 3, 2)" should "equals to \"ccc\nccc\"" in {
    val e = Element.elem('c', 3, 2)
    e.toString shouldEqual "ccc\nccc"
  }

  "above" should "concat element vertically" in {
    val e = Element.elem("aaa").above(Element.elem("bbb"))
    e.toString shouldEqual "aaa\nbbb"
  }

  "beside" should "concat element horizontally" in {
    val e = Element.elem("aaa").beside(Element.elem("bbb"))
    e.toString shouldEqual "aaabbb"
  }
}
