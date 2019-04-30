package net.hkdnet.numeric

import org.scalatest._

class CurrencySpec extends FlatSpec with Matchers {
  "US.make(1)" should "equals to 1 cent" in {
    val usd = US.make(1)
    usd.amount shouldEqual (1)
    usd.toString shouldEqual ("0.01 USD")
  }

  "US.make(100)" should "equals to 1 $" in {
    val usd = US.make(100)
    usd.amount shouldEqual (100)
    usd.toString shouldEqual ("1.00 USD")
  }

  "US.make(1) + US.make(2)" should "equals to 3 cents" in {
    val a = US.make(1)
    val b = US.make(2)
    val c = a + b
    c.amount shouldEqual (3)
    c.toString shouldEqual ("0.03 USD")
  }

  "US.make(10) * 1.2" should "equals to 12 cents" in {
    val a = US.make(10)
    val b = a * 1.2
    b.amount shouldEqual (12)
  }
}
