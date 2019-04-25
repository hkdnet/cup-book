package net.hkdnet.numeric

import org.scalatest._

class RationalSpec extends FlatSpec with Matchers {
  "n/1" should "equals to n" in {
    for (i <- 1 to 100) {
      val n = new Rational(i, 1)
      n shouldEqual new Rational(i)
    }
  }

  "2/4" should "equals to 1/2" in {
    new Rational(2, 4) shouldEqual new Rational(1, 2)
  }

  "4/2" should "equals to 2" in {
    new Rational(4, 2) shouldEqual new Rational(2)
  }

  "1/3 + 1/2" should "equals to 5/6" in {
    val r = new Rational(1, 3) + new Rational(1, 2)
    r shouldEqual new Rational(5, 6)
  }

  "2/3 * 1/2" should "equals to 1/3" in {
    val r = new Rational(2, 3) * new Rational(1, 2)
    r shouldEqual new Rational(1, 3)
  }
}
