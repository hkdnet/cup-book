package net.hkdnet.gui

import net.hkdnet.parser.FormulaParsers
import net.hkdnet.scell.{Application, Coord, Empty, Formula, Number, Textual, Range}

import scala.collection.mutable
import scala.swing._
import scala.swing.event.TableUpdated

trait Evaluator {
  this: Model =>
  type Op = List[Double] => Double

  val operations = new mutable.HashMap[String, Op]()

  def evaluate(e: Formula): Double = try {
    e match {
      case Coord(row, column) => cells(row)(column).value
      case Number(v) => v
      case Textual(_) => 0
      case Application(function, arguments) => {
        val argValues = arguments flatMap evalList
        operations(function)(argValues)
      }
    }
  } catch {
    case _: Exception => Double.NaN
  }

  private def evalList(e: Formula): List[Double] = e match {
    case Range(_, _) => references(e) map (e => e.value)
    case _ => List(evaluate(e))
  }

  def references(e: Formula): List[Cell] = e match {
    case Coord(row, column) => List(cells(row)(column))
    case Range(Coord(r1, c1), Coord(r2, c2)) => {
      for (r <- (r1 to r2).toList; c <- c1 to c2) yield cells(r)(c)
    }
    case Application(f, args) => args flatMap references
    case _ => List()
  }
}

trait Arithmetic {
  this: Evaluator =>
  operations += (
    "add" -> { case List(x, y) => x + y },
    "sub" -> { case List(x, y) => x - y },
    "div" -> { case List(x, y) => x / y },
    "mul" -> { case List(x, y) => x * y },
    "mod" -> { case List(x, y) => x % y },
    "sum" -> { xs => (0.0 /: xs) (_ + _) },
    "prod" -> { xs => (1.0 /: xs) (_ * _) }
  )
}


class Model(val height: Int, val width: Int) extends Evaluator with Arithmetic {
  case class ValueChanged(cell: Cell) extends event.Event
  case class Cell(row: Int, column: Int) extends Publisher {
    private var v: Double = 0
    private var f: Formula = Empty

    def value: Double = v

    def value_=(w: Double) = {
      if (!(v == w || v.isNaN && w.isNaN) ){
        v = w
        publish(ValueChanged(this))
      }
    }

    def formula = f

    def formula_=(w: Formula) = {
      for (c <- references(formula)) deafTo(c)
      f = w
      for (c <- references(formula)) listenTo(c)
      value = evaluate(f)
    }

    override def toString: String = formula match {
      case Textual(s) => s
      case _ => value.toString
    }

    reactions += {
      case ValueChanged(_) => value = evaluate(f)
    }
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
          cells(row)(column).formula = FormulaParsers.parse(userData(row, column))
      }
      case ValueChanged(cell) => {
        updateCell(cell.row, cell.column)
      }
    }

    for (row <- cells; cell <- row) listenTo(cell)
  }

  val rowHeader =
    new ListView((0 until height) map (_.toString)) {
      fixedCellWidth = 30
      fixedCellHeight = table.rowHeight
    }

  viewportView = table
  rowHeaderView = rowHeader
}
