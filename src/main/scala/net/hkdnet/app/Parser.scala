package net.hkdnet.app

import net.hkdnet.parser._

object Parser {
  def expr(input: String) = {
    val result = ExprParser.parseAll(ExprParser.expr, input)

    if (result.successful) {
      result.get
    } else {
      throw new Exception(result.toString)
    }
  }
}
