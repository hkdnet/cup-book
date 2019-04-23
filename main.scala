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

object Main {
  def main(args: Array[String]): Unit = {
    val a = new Rational(1, 4)
    val b = new Rational(3, 4)
    println(new Rational(1) / 4 * 3 + new Rational(1) / 2)
  }
}
