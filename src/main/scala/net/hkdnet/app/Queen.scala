package net.hkdnet.app

object Queen {
  def solve(n: Int): List[List[(Int, Int)]] = {
    def placeQueens(k: Int): List[List[(Int, Int)]] =
      if (k == 0)
        List(Nil)
      else
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens)
        } yield queen :: queens

    placeQueens(n)
  }

  def isSafe(q: (Int, Int), queens: List[(Int, Int)]): Boolean = {
    queens.forall { tmp =>
      val (x, y) = tmp

      x != q._1 && y != q._2 &&
        (y - q._2).abs != (x - q._1).abs // dy/dx != 1
    }
  }

  def show(queens: List[(Int, Int)]) = {
    val n = queens.size
    queens.sorted.foreach(q => {
      (1 to n).foreach(x => {
        if (x == q._2)
          print("x")
        else
          print("_")
      })
      println()
    })
  }
}
