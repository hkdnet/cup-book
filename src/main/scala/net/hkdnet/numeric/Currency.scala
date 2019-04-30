package net.hkdnet.numeric

abstract class CurrencyZone {
  type Currency <: AbstractCurrency

  def make(x: Long): Currency
  val CurrencyUnit: Currency

  abstract class AbstractCurrency {
    val amount: Long


    def designation: String

    override def toString: String = {
      val f = (amount.toDouble / CurrencyUnit.amount.toDouble) formatted ("%." + decimals(CurrencyUnit.amount) + "f")
      f + " " + designation
    }

    private def decimals(n: Long): Int = {
      if (n == 1) 0 else 1 + decimals(n / 10)
    }

    def +(that: Currency): Currency = make(this.amount + that.amount)

    def *(x: Double): Currency = make((this.amount * x).toLong)
  }

}

object US extends CurrencyZone {

  abstract class Dollar extends AbstractCurrency {
    def designation: String = "USD"
  }

  type Currency = Dollar

  def make(cents: Long) = new Dollar {
    val amount = cents
  }

  val Sent = make(1)
  val Dollar = make(100)
  val CurrencyUnit = Dollar
}

object Europe extends CurrencyZone {

  abstract class Euro extends AbstractCurrency {
    def designation: String = "EUR"
  }

  type Currency = Euro

  def make(cents: Long) = new Euro {
    val amount = cents
  }

  val Cent = make(1)
  val Euro = make(100)
  val CurrencyUnit = Euro
}


object Japan extends CurrencyZone {

  abstract class Yen extends AbstractCurrency {
    def designation: String = "JPY"
  }

  type Currency = Yen

  def make(yen: Long) = new Yen {
    val amount = yen
  }

  val Yen = make(1)
  val CurrencyUnit = Yen
}
