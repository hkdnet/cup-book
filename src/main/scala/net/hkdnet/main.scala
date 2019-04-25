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

class Layout

sealed abstract class Expr

case class Var(name: String) extends Expr

case class Number(n: Int) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, lhs: Expr, rhs: Expr) extends Expr

abstract class Element {
  def contents: Array[String]

  def width: Int = if (height == 0) 0 else contents(0).length

  def height: Int = contents.length

  def above(that: Element): Element = {
    Element.elem(contents ++ that.contents)
  }

  def beside(that: Element): Element = {
    Element.elem(
      for (
        (line1, line2) <- this.contents zip that.contents
      ) yield line1 + line2
    )
  }

  override def toString: String = contents mkString("\n")
}

object Element {
  private class ArrayElement(val contents: Array[String]) extends Element {
  }

  private class LineElement(s: String) extends Element() {
    override def contents: Array[String] = Array(s)

    override def width: Int = s.length

    override def height: Int = 1
  }

  private class UniformElement(ch: Char, override val width: Int, override val height: Int) extends Element {
    private val line = ch.toString * width

    def contents = Array.fill(height)(line)
  }

  def elem(contents: Array[String]): Element = new ArrayElement(contents)
  def elem(chr: Char, width: Int, height: Int):Element = new UniformElement(chr, width, height)
  def elem(line: String): Element = new LineElement(line)
}

object Main {
  def main(args: Array[String]): Unit = {
    println(new Rational(1) / 4 * 3 + new Rational(1) / 2)

    // (x + y) * z + 1
    val expr = BinOp(
      "+",
      BinOp("*",
        BinOp("+", Var("x"), Var("y")),
        Var("z")),
      Number(1)
    )
    println(expr)
  }

  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case UnOp("-", Number(e)) if e < 0 => simplify(Number(-e))
    case BinOp("+", e, Number(0)) => simplify(e)
    case BinOp("*", e, Number(1)) => simplify(e)
    case _ => expr
  }
}
