package net.hkdnet

import net.hkdnet.numeric.Rational
import net.hkdnet.numeric._

object Main {
  def main(args: Array[String]): Unit = args match {
    case Array() => println("USAGE: action")
    case Array("rational") => {
      println(new Rational(1) / 4 * 3 + new Rational(1) / 2)
    }
    case Array("spiral") => {

    }
    case Array("expr") => {
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
  }
}
