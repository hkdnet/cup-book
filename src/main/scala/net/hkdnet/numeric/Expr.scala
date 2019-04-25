package net.hkdnet.numeric

sealed abstract class Expr

object Expr {
  def simplify(expr: Expr): Expr = expr match {
    case UnOp("-", UnOp("-", e)) => simplify(e)
    case UnOp("-", Number(e)) if e < 0 => simplify(Number(-e))
    case BinOp("+", e, Number(0)) => simplify(e)
    case BinOp("*", e, Number(1)) => simplify(e)
    case _ => expr
  }
}

case class Var(name: String) extends Expr

case class Number(n: Int) extends Expr

case class UnOp(operator: String, arg: Expr) extends Expr

case class BinOp(operator: String, lhs: Expr, rhs: Expr) extends Expr