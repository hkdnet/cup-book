package net.hkdnet.numeric

class Rational(a: Int, b: Int) {
  require(b != 0)
  private val g = gcd(a, b)
  val numer = a / g
  val denom = b / g

  def this(n: Int) = this(n, 1)

  override def toString: String = if (denom == 1) numer.toString else numer + "/" + denom

  private def gcd(x: Int, y: Int): Int = {
    if (x < y) gcd(y, x)
    if (y == 0) return x
    return gcd(y, x % y)
  }

  def +(that: Rational): Rational = new Rational(numer * that.denom + that.numer * denom, denom * that.denom)

  def +(n: Int): Rational = this + new Rational(n)

  def *(that: Rational): Rational = new Rational(numer * that.numer, denom * that.denom)

  def *(n: Int): Rational = new Rational(numer * n, denom)

  def /(that: Rational): Rational = this * new Rational(that.denom, that.numer)

  def /(n: Int): Rational = this * new Rational(1, n)
}
