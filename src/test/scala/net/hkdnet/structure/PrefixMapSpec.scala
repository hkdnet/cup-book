package net.hkdnet.structure

import org.scalatest._

class PrefixMapSpec extends FlatSpec with Matchers {
  "PrefixMap" should "create a new map" in {
    val m = PrefixMap.empty[Int]
    m += ("abc" -> 0)
    m("abc") shouldEqual 0
  }
  "withPrefix" should "return the subtree" in {
    val m = new PrefixMap[Int]
    m += ("abc" -> 0)
    m += ("abd" -> 1)
    m += ("al" -> 2)

    m.get("ab") shouldEqual None

    val withAb = m withPrefix "ab"
    withAb.get("al") shouldEqual None
    withAb.get("abc") shouldEqual None
    withAb.get("c") shouldEqual Some(0)

    m -= ("abc")
    // if an entry is removed from prefix map, we cannot get it from derived maps.
    withAb.get("c") shouldEqual None

    withAb += ("e" -> 3)
    // if an entry is added to derived maps, we can get it from the original map.
    m.get("abe") shouldEqual Some(3)
  }
}
