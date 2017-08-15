class Cell(v: Int, c: String, _row: Int, _col: Int) {
  val value: Int = v
  val color: String = c
  val row = _row
  val col = _col
}

class PredefinedCell(value: Int, c: String, row: Int, col: Int) extends Cell(value, c, row, col) {
  override def toString: String = s"($value:$c:($row,$col))"
}

/**
  * --------------------- --------------------- --------------------- --------------------- ---------------------
  */
val data =
  """(),2@BROWN,(),()
    |(),1@RED,(),()
    |3@BLUE,(),(),2@YELLOW
    |(),(),2@GREEN,()""".stripMargin
val rows = 4
val cols = 4

val dummy = Vector.tabulate(rows, cols) {
  (r, j) => Option[Cell](null)
}

val predefinedCells = for {
  x <- {
    val r = data.split("\n");
    r zip (0 to rows)
  };
  y <- {
    var c = x._1.split(",");
    c zip (0 to cols)
  }
  if y._1 != "()"
} yield {
  val (n, c) = (y._1.split("@")(0), y._1.split("@")(1))
  new PredefinedCell(n.toInt, c, x._2, y._2)
}

// update the board based on read values. Board is defined.
val board = predefinedCells.foldLeft(dummy)((myB, nextCell) =>
  myB.updated(nextCell.row, myB(nextCell.row).updated(nextCell.col, Some(nextCell)))
)


//println(calcNeighbours(board, predefinedCells(4)))
//println(calcNeighbours(board, new PredefinedCell(1, "L", 2, 1)))
val xx = chooseCombinations(board, calcNeighbours(board, predefinedCells(4)),
  Nil,List[List[(Int, Int)]](), predefinedCells(4).value, predefinedCells(4).value).map (x => x.sorted).toSet
println(xx.mkString("\n"))
/**
  * Returns a list of directional neighbours
  * @param board
  * @param cell
  * @return
  */
def calcNeighbours(board: Vector[Vector[Option[Cell]]], cell: PredefinedCell): List[List[(Int, Int)]] = {
  val rows = board.size
  val cols = board(0).size

  println(cell)
  val minRow = Math.max(0, cell.row - cell.value)
  val up: List[(Int, Int)] = (minRow until cell.row).foldLeft(List[(Int, Int)]())((b, i) => (i, cell.col) :: b)

  val maxRow = Math.min(rows - 1, cell.row + cell.value)
  val down: List[(Int, Int)] = (cell.row + 1 to maxRow).foldRight(List[(Int, Int)]())((i, b) => (i, cell.col) :: b)

  val minCol = Math.max(0, cell.col - cell.value)
  val left: List[(Int, Int)] = (minCol until cell.col).foldLeft(List[(Int, Int)]())((b, i) => (cell.row, i) :: b)

  val maxCol = Math.min(cols - 1, cell.col + cell.value)
  val right: List[(Int, Int)] = (cell.col + 1 to maxCol).foldRight(List[(Int, Int)]())((i, b) => (cell.row, i) :: b)

  List(up, down, left, right).filter(_.nonEmpty)
}

def chooseCombinations(
                       board: Vector[Vector[Option[Cell]]],
                       l: List[List[(Int,Int)]],
                       local_accumulator: List[(Int, Int)],
                       result_accumulator: List[List[(Int, Int)]],
                       max_depth: Int,
                       remaining: Int) : List[List[(Int,Int)]] = {

  if (l.isEmpty) result_accumulator;
  else {
     l.zipWithIndex.foldLeft(result_accumulator) ( (accum, nextList) => {
        // add the data
       if (remaining == 0) {
         System.err.println("Added " + local_accumulator)
         return accum :+ local_accumulator
       }
       val element_idx = nextList._2
       // check if this is currently available on the board
       board()


       // for each one of the lists, we can take 1 element
       val local_accum = {
         if (max_depth == remaining) List(nextList._1.head) else local_accumulator :+ nextList._1.head
       }
       val remainingElements = {
          // drop the list completely if empty
         if (nextList._1.size == 1) l.take(element_idx) ++ l.drop(element_idx+1) else l.updated(element_idx, l(element_idx).drop(1))
       }
       chooseCombinations(board, remainingElements,local_accum,accum, max_depth,remaining-1)


     }
     )

  }

}












def printBoard(board: Array[Array[Option[Cell]]]): Unit = {
  for {
    x <- (0 until rows);
    y <- (0 until cols)
  } yield {
    (board(x)(y): Option[Cell]) match {
      case Some(c: Cell) => print(" " + c)
      case None => print("''")
    }
    print(",")
    if (y == cols - 1) println()
  }

}





//
//val elements = List("a", "b", "c")
//
//val twoDimenstionalList: List[List[String]] = List.empty[List[String]]
//
//val res = for(element <- elements) yield twoDimenstionalList ::: List(element)
//val rres = for(element <- elements) yield List(element)
//Vector.fill(rows,cols)(
//
//  new PredefinedCell(2,"blue",0,0)
//)
//
//elements.map(List(_))
//elements.map(x=>List(x))
//var c = (for (i <- 0 to 99; j <- 0 to 99) yield (i,j) -> 0)
//c.length