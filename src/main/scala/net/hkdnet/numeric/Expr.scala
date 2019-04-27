package net.hkdnet.numeric

import net.hkdnet.layout.Element

sealed abstract class Expr

object Expr {
  private val opGroups =
    Array(
      Set("|", "||"),
      Set("&", "&&"),
      Set("^"),
      Set("==", "!="),
      Set("<", "<=", ">", ">="),
      Set("+", "-"),
      Set("*", "%")
    )

  private val precedence = {
    val assocs = for {
      i <- 0 until opGroups.length
      op <- opGroups(i)
    } yield op -> i
    assocs.toMap
  }

  private val unaryPrecedence = precedence.size
  private val fracPrecdedence = -1

  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case UnOp("-", Number(e)) if e < 0 => simplify(Number(-e))
    case BinOp("+", e, Number(0)) => simplify(e)
    case BinOp("*", e, Number(1)) => simplify(e)
    case UnOp(o, e) => UnOp(o, simplify(e))
    case BinOp(o, l, r) => BinOp(o, simplify(l), simplify(r))
    case _ => expr
  }

  def format(expr: Expr): Element = format(expr, 0)

  private def format(expr: Expr, enclPrec: Int): Element = expr match {
    case Var(x) => Element.elem(x)
    case Number(n) => {
      def stripDot(s: String): String = {
        if (s.endsWith(".0")) s.substring(0, s.length - 2) else s
      }
      Element.elem(stripDot(n.toString))
    }
    case UnOp(o, e) => Element.elem(o) beside format(e, unaryPrecedence)
    case BinOp("/", l, r) => {
      val top = format(l, fracPrecdedence)
      val bot = format(r, fracPrecdedence)
      val line = Element.elem('-', top.width max bot.width , 1)
      val frac = top above line above bot
      if (enclPrec !=  fracPrecdedence) frac
      else Element.elem(" ") beside frac beside Element.elem(" ")
    }
    case BinOp(o, l, r) => {
      val opPrec = precedence(o)
      val ll = format(l, opPrec)
      val rr = format(r, opPrec + 1)
      val oper = ll beside Element.elem(" " + o + " ") beside rr
      if (enclPrec <= opPrec) oper
      else Element.elem("(") beside oper beside Element.elem(")")
    }
    case _ => throw new RuntimeException("unknwon " + expr)
  }
}

case class Var(name: String) extends Expr

case class Number(n: Double) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, lhs: Expr, rhs: Expr) extends Expr
