class Board( val boardStr: String) {

  val predefinedCells = for {
    x <- {
      boardStr.split("\n").zipWithIndex;
    };
    y <- {
      x._1.split(",").zipWithIndex;
    }
    if y._1.trim != "()"
  } yield {
    val (n, c) = (y._1.split("@")(0), y._1.split("@")(1))
    new PredefinedCell(n.toInt, c, x._2, y._2)

  }
  val rows = boardStr.split("\n").length
  val cols = boardStr.split("\n")(0).split(",").length

  private val dummy = Vector.tabulate(rows, cols) {
    (r, j) => Option[Cell](null)
  }
  // update the board based on read values. Board is defined.
  val board = predefinedCells.foldLeft(dummy)((myB, nextCell) =>
    myB.updated(nextCell.row, myB(nextCell.row).updated(nextCell.col, Some(nextCell)))
  )


  /**
    * returns whether a cell is currently occupied by something
    */
  def isOccupied(row:Int, col:Int): Boolean = {
      board(row)(col) match {
        case None => false
        case Some(_) => true
      }
  }

}

case class Cell(value: Int, color: String, row: Int, col: Int) {}

class PredefinedCell(value: Int, c: String, val _row: Int, val _col: Int) extends Cell(value, c, _row, _col) {
  override def toString: String = s"($value:$c:($row,$col))"
}


















object Main extends App {
  def testNeighbours(): Unit = {
    val data =
      """(),2@BROWN,(),()
        |(),1@RED,(),()
        |3@BLUE,(),(),2@YELLOW
        |(),(),2@GREEN,()""".stripMargin
    val boardObj = new Board(data)
    val cell = boardObj.predefinedCells(4)
    val neighbours: List[List[(Int, Int)]] = Board.calcNeighbours(boardObj, cell)
    assert( neighbours == List(List((2,2), (1,2)), List((3,1), (3,0)), List((3,3))) )
  }

  def testAllCombinations(): Unit = {
    val data =
      """(),2@BROWN,(),()
        |(),1@RED,(),()
        |3@BLUE,(),2@YELLOW,()
        |(),(),2@GREEN,()""".stripMargin
    val boardObj = new Board(data)

    val cell = boardObj.predefinedCells(4)
    val combinations = Board.chooseCombinations(boardObj, Board.calcNeighbours(boardObj,cell),
      Nil,List[List[(Int, Int)]](), cell.value, cell.value).map (x => x.sorted).toSet
    //println(combinations.mkString("\n"))
    val expected = Set(List((3,1), (3,3)), List((3,0), (3,1)))
    assert( combinations == expected, s"Expected $expected. Seen: $combinations" )

    // test the second element
    val middleElement = boardObj.predefinedCells(1)
    val c = Board.chooseCombinations(boardObj, Board.calcNeighbours(boardObj,middleElement),
      Nil,List[List[(Int, Int)]](), middleElement.value, middleElement.value).map (x => x.sorted).toSet
    val expected2 = Set(List((2,1)), List((1,0)), List((1,2)))
    assert( c == expected2, s"Expected $expected2. Seen: $c" )

    // test third element (2,2) is not working
    val sideElement = boardObj.predefinedCells(2)
    val s = Board.chooseCombinations(boardObj, Board.calcNeighbours(boardObj,sideElement),
      Nil,List[List[(Int, Int)]](), sideElement.value, sideElement.value).map (x => x.sorted).toSet
    val expected3 = Set(List((2,1)), List((1,0)), List((1,2)))
    assert( s == expected3, s"Expected $expected3. Seen: $s" )


  }

  testNeighbours()
  testAllCombinations()


}

object Board {
  /**
    * Returns a list of directional neighbours
    * @param cell
    * @return
    */
  def calcNeighbours(boardObj: Board, cell: PredefinedCell): List[List[(Int, Int)]] = {
    val board = boardObj.board

    val rows = boardObj.rows
    val cols = boardObj.cols

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
  def printBoard(board: Board): Unit = {
    val data = board.board
    for {
      x <- 0 until board.rows;
      y <- 0 until board.cols
    } yield {
      (data(x)(y): Option[Cell]) match {
        case Some(c: Cell) => print(" " + c)
        case None => print("''")
      }
      print(",")
      if (y == board.cols - 1) println()
    }

  }

  def chooseCombinations(
                          boardObj: Board,
                          l: List[List[(Int,Int)]],
                          local_accumulator: List[(Int, Int)],
                          result_accumulator: List[List[(Int, Int)]],
                          max_depth: Int,
                          remaining: Int) : List[List[(Int,Int)]] = {
    val board = boardObj.board
    if (l.isEmpty) result_accumulator;
    else {
      l.zipWithIndex.foldLeft(result_accumulator) ( (accum, nextList) => {
        // add the data
        if (remaining == 0) {
          //System.err.println("Added " + local_accumulator)
          return accum :+ local_accumulator
        }
        val element_idx = nextList._2

        val firstListElement = nextList._1.head
        // cannot take the element because something it is already taken
        if (boardObj.isOccupied(firstListElement._1, firstListElement._2)) {
          return chooseCombinations(boardObj,  l.take(element_idx) ++ l.drop(element_idx+1),local_accumulator,accum, max_depth,remaining)
        }

        // can take the element

        // for each one of the lists, we can take 1 element
        val local_accum = {
          if (max_depth == remaining) List(firstListElement) else local_accumulator :+ firstListElement
        }
        val remainingElements = {
          // drop the list completely if empty
          if (nextList._1.size == 1) l.take(element_idx) ++ l.drop(element_idx+1) else l.updated(element_idx, l(element_idx).drop(1))
        }
        chooseCombinations(boardObj, remainingElements,local_accum,accum, max_depth,remaining-1)


      }
      )

    }

  }

}


