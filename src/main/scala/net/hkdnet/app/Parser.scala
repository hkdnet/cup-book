package net.hkdnet.app

import net.hkdnet.numeric.Expr
import net.hkdnet.parser._


object Parser {
  def expr(input: String) = {
    val result = ExprParser.parseAll(ExprParser.expr, input) map (e => Expr.format(e))

    if (result.successful) {
      println(result.get)
    } else {
      println(result)
    }
  }
}
