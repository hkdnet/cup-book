package net.hkdnet.gui

import scala.swing._
import scala.swing.event.TableUpdated
import scala.util.parsing.combinator.RegexParsers

trait Formula

case class Coord(row: Int, column: Int) extends Formula {
  override def toString: String = ('A' + column).toChar.toString + row
}

case class Range(c1: Coord, c2: Coord) extends Formula {
  override def toString: String = c1 + ":" + c2
}

case class Number(value: Double) extends Formula {
  override def toString: String = value.toString
}

case class Textual(value: String) extends Formula {
  override def toString: String = value
}

case class Application(function: String, arguments: List[Formula]) extends Formula {
  override def toString: String = function + arguments.mkString("(", ",", ")")
}

object Empty extends Textual("")

object FormulaParser extends RegexParsers {
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

  def textual: Parser[Textual] = "" ^^ (_=>Empty) | """[^=].*""".r ^^ Textual

  def formula: Parser[Formula] = number | textual | "=" ~> expr

  def parse(input: String): Formula =
    parseAll(formula, input) match {
      case Success(result, _) => result
      case f: NoSuccess => Textual("[" + f.msg + "]")
    }
}

class Model(val height: Int, val width: Int) {

  case class Cell(row: Int, column: Int) {
    var formula: Formula = Empty

    override def toString: String = formula.toString
  }

  val cells = Array.ofDim[Cell](height, width)
  for (i <- 0 until height; j <- 0 until width)
    cells(i)(j) = new Cell(i, j)
}

class SpreadSheet(val height: Int, val width: Int) extends ScrollPane {
  val cellModel = new Model(height, width)

  import cellModel._

  val table = new Table(height, width) {
    rowHeight = 25
    autoResizeMode = Table.AutoResizeMode.Off
    showGrid = true
    gridColor = new Color(150, 150, 150)

    override def rendererComponent(isSelected: Boolean, focused: Boolean, row: Int, column: Int): Component = {
      if (hasFocus) new TextField(userData(row, column))
      else
        new Label(cells(row)(column).toString) {
          xAlignment = Alignment.Right
        }
    }

    def userData(row: Int, column: Int): String = {
      val v = this (row, column)
      if (v == null) "" else v.toString
    }

    reactions += {
      case TableUpdated(table, rows, column) => {
        for (row <- rows)
          cells(row)(column).formula = FormulaParser.parse(userData(row, column))
      }
    }
  }

  val rowHeader =
    new ListView((0 until height) map (_.toString)) {
      fixedCellWidth = 30
      fixedCellHeight = table.rowHeight
    }

  viewportView = table
  rowHeaderView = rowHeader
}
