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

  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case UnOp("-", Number(e)) if e < 0 => simplify(Number(-e))
    case BinOp("+", e, Number(0)) => simplify(e)
    case BinOp("*", e, Number(1)) => simplify(e)
    case UnOp(o, e) => UnOp(o, simplify(e))
    case BinOp(o, l, r) => BinOp(o, simplify(l), simplify(r))
    case _ => expr
  }

  def format(expr: Expr): Element = expr match {
    case Var(x) => Element.elem(x)
    case Number(n) => Element.elem(n.toString)
    case UnOp(o, e) => Element.elem(o) beside format(e)
    case BinOp(o, l, r) => Element.elem("(") beside format(l) beside Element.elem(o) beside format(r) beside Element.elem(")")
    case _ => throw new RuntimeException("unknwon " + expr)
  }
}

case class Var(name: String) extends Expr

case class Number(n: Int) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, lhs: Expr, rhs: Expr) extends Expr
