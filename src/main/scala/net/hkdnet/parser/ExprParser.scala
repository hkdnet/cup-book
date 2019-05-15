package net.hkdnet.parser

import scala.util.parsing.combinator._
import net.hkdnet.numeric._

object ExprParser extends JavaTokenParsers {
  def expr: Parser[Expr] = term~rep("+"~term | "-"~term) ^^ {
    case t~xs => {
      xs.foldLeft(t) {
        case(lhs, op~rhs) => BinOp(op, lhs, rhs)
      }
    }
  }
  def term: Parser[Expr] = factor~rep("*"~factor | "/"~factor) ^^ {
    case f ~ xs => {
      xs.foldLeft(f) {
        case (lhs, op~rhs) => BinOp(op, lhs, rhs)
      }
    }
  }
  def factor: Parser[Expr] = floatingPointNumber^^(e => Number(e.toDouble)) | "("~>expr<~")"
}
