package net.hkdnet.app

import net.hkdnet.parser._

object Parser {
  def expr(input: String) = {
    println(ExprParser.parseAll(ExprParser.expr, input))
  }
}
