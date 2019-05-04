package net.hkdnet

import net.hkdnet.numeric._
import net.hkdnet.structure._
import net.hkdnet.app._

object OrderedMergeSort {
  def orderedMergeSort[T <: Ordered[T]](xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] = {
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x::xs1, y::ys1) =>
          if (x < y) x :: merge(xs1, ys)
          else y:: merge(xs, ys1)
      }
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (ys, zs) = xs.splitAt(n)
      merge(orderedMergeSort(ys), orderedMergeSort(zs))
    }
  }
}

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
    case Array("sort") => {
      import net.hkdnet.data.Person

      val persons = List(
        new Person("Miki", "Hoshii"),
        new Person("Ritsuko", "Akiduki"),
        new Person("Makoto", "Kikuchi"),
      )

      println(persons)
      println("---")
      println(OrderedMergeSort.orderedMergeSort(persons))
    }
  }
}
