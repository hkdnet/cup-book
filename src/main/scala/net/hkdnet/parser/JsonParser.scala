package net.hkdnet.parser


import scala.util.parsing.combinator._

object JsonParser extends JavaTokenParsers {
  def value: Parser[Any] = (
        "true"^^(_ => true)
      | "false"^^(_=> false)
      | "null"^^(_ => null)
      | floatingPointNumber^^(e => e.toDouble)
      | stringLiteral
      | array
      | obj
    )

  def array: Parser[List[Any]] = "[" ~> arrayValues <~ "]"

  def arrayValues: Parser[List[Any]] = repsep(value, ",")^^(_.toList)

  def obj: Parser[Map[String, Any]] = "{" ~> repsep(objItem, ",") <~ "}"^^(_.toMap)

  def objItem: Parser[(String, Any)] = stringLiteral ~ ":" ~ value^^ {case k~_~v => (k, v)}
}
