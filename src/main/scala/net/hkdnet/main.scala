package net.hkdnet

import net.hkdnet.numeric._
import net.hkdnet.structure._
import net.hkdnet.app._

object Main {
  def main(args: Array[String]): Unit = args match {
    case Array() => println("USAGE: action")
    case Array("rational") => {
      println(new Rational(1) / 4 * 3 + new Rational(1) / 2)
    }
    case Array("spiral") => {
      println(
        Spiral.spiral(14, 0)
      )
    }
    case Array("expr") => {
      // (x + y) * z + 1
      val expr = BinOp(
        "+",
        BinOp("*",
          BinOp("+", Var("x"), Var("y")),
          Var("z")),
        Number(1)
      )
      println(Expr.format(expr))
      println(Expr.format(Number(1.0)))
      println(Expr.format(UnOp("-", Number(1))))
      println(Expr.format(BinOp("/", Number(1), Var("x"))))
      println(Expr.format(
        BinOp("/", Var("y"),
        BinOp("/", Number(1), Var("x")))))
    }
    case Array("queue") => {
      val q = Queue[Int]()
      println(q)
      println(q.enqueue(1))
      println(q.enqueue(1).enqueue(2))
      println(Queue(1, 2))
    }
    case Array("sorter") => {
      OrderedMergeSort.examples
    }
  }
}
