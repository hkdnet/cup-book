package net.hkdnet

import java.io.FileReader

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
      import net.hkdnet.app.Parser
      // (x + y) * z + 1
      val expr = Parser.expr("(x + y) * z + 1")
      println(Expr.format(expr))
      println(Expr.format(Number(1.0)))
      println(Expr.format(UnOp("-", Number(1))))
      println(Expr.format(BinOp("/", Number(1), Var("x"))))
      println(Expr.format(Parser.expr("y/(1/x)")))
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
    case Array("queen") => {
      val answers = Queen.solve(8)
      println(answers(0).sorted)
      Queen.show(answers(0).sorted)
    }
    case Array("dna") => {
      import net.hkdnet.dna._
      val xs = List(A, G, T, A)
      println(DNA.fromSeq(xs))
      println(DNA(A, U, G, T, T, A))
      println(DNA(A, U, G, T, T, A).take(3))
      val dna = DNA(T, U, G, G, T)
      println(dna ++ dna)
      dna foreach println
    }
    case Array("parse-expr", expr) => {
      import net.hkdnet.app.Parser
      println(Expr.format(Parser.expr(expr)))
    }
    case Array("parse-json", filename) => {
      val f = new FileReader(filename)
      import net.hkdnet.parser.JsonParser
      try println(JsonParser.parseAll(JsonParser.value, f))
      finally f.close()
    }
  }
}
