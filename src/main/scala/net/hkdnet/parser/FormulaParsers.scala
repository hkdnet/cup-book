package net.hkdnet.parser

import scala.util.parsing.combinator._
import net.hkdnet.data.{Formula,Coord, Range, Number, Application, Textual, Empty}

object FormulaParsers extends RegexParsers {
  def ident: Parser[String] = """[a-zA-Z_]\w*""".r

  def decimal: Parser[String] = """-?\d+(\.\d*)?""".r

  def cell: Parser[Coord] =
    """[A-Za-z]\d+""".r ^^ { e =>
      val column = e.charAt(0).toUpper - 'A'
      val row = e.substring(1).toInt
      Coord(row, column)
    }

  def range: Parser[Range] =
    cell ~ ":" ~ cell ^^ { case c1 ~ ":" ~ c2 => Range(c1, c2) }

  def number: Parser[Number] = decimal ^^ (e => Number(e.toDouble))

  def application: Parser[Application] =
    ident ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
      case f ~ "(" ~ params ~ ")" => Application(f, params)
    }

  def expr: Parser[Formula] = range | cell | number | application

  def textual: Parser[Textual] = """[^=].*""".r ^^ Textual | """\A\z""".r ^^ (_=>Empty)

  def formula: Parser[Formula] = number | textual | "=" ~> expr

  def parse(input: String): Formula =
    parseAll(formula, input) match {
      case Success(result, _) => result
      case f: NoSuccess => Textual("[" + f.msg + "]")
    }
}
