// add the data

class Board(val board: Vector[Vector[Option[Cell]]], val predefinedCells: Array[PredefinedCell]) {
  val rows = board.length
  val cols = board(0).length

  /**
    * returns whether a cell is currently occupied by something
    */
  def isOccupied(row: Int, col: Int): Boolean = {
    board(row)(col) match {
      case None => false
      case Some(_) => true
    }
  }
}

case class Cell(value: Int, color: String, row: Int, col: Int) {
  override def toString: String = s"($value:$color)"
}

class PredefinedCell(value: Int, c: String, val _row: Int, val _col: Int) extends Cell(value, c, _row, _col) {
  override def toString: String = s"P($value:$color)"
}

object Main extends App {
  def testNeighbours(): Unit = {
    val data =
      """(),2@BROWN,(),()
        |(),1@RED,(),()
        |3@BLUE,(),(),2@YELLOW
        |(),(),2@GREEN,()""".stripMargin
    val boardObj = Board.parseString(data)
    val cell = boardObj.predefinedCells(4)
    val neighbours: List[List[(Int, Int)]] = Board.calcNeighbours(boardObj, cell)
    assert(neighbours == List(List((2, 2), (1, 2)), List((3, 1), (3, 0)), List((3, 3))))
  }


  def testAllCombinations(): Unit = {
    val data =
      """(),2@BROWN,(),()
        |(),1@RED,(),()
        |3@BLUE,(),2@YELLOW,()
        |(),(),2@GREEN,()""".stripMargin

    val boardObj = Board.parseString(data)
    val cell = boardObj.predefinedCells(4)
    val combinations = Board.chooseCombinations(boardObj, cell)

    val expected = Set(List((3, 1), (3, 3)), List((3, 0), (3, 1)))

    assert(combinations == expected, s"Expected $expected. Seen: $combinations")

    // test the second element
    val middleElement = boardObj.predefinedCells(1)
    val c = Board.chooseCombinations(boardObj, middleElement)
    val expected2 = Set(List((2, 1)), List((1, 0)), List((1, 2)))
    assert(c == expected2, s"Expected $expected2. Seen: $c")

    // test third element (2,2) is not working
    val sideElement = boardObj.predefinedCells(2)
    val s = Board.chooseCombinations(boardObj, sideElement)
    val expected3 = Set(List((0, 0), (1, 0), (3, 0)), List((0, 0), (1, 0), (2, 1)), List((1, 0), (2, 1), (3, 0)))
    assert(s == expected3, s"Expected $expected3. Seen: $s")
  }


  testNeighbours()
  testAllCombinations()
  solveBoard()


  def solveBoard() = {
    val data =
      """(),4@BL,(),(),()
        |1@G,(),(),(),4@LBLUE
        |(),(),(),3@R,()
        |(),(),1@U,(),()
        |3@Y,(),(),2@R,()""".stripMargin
    val boardObj = Board.parseString(data)

    // could have several solutiions
    def helper(allSolutions: Set[Board], board: Board, predefinedList: Array[PredefinedCell]): Set[Board] = {
      // gone through all predefined cells. Question is, is board ok
      if (predefinedList.isEmpty) {
        return allSolutions + board
      }

      // what if the board is already solved
      val next = predefinedList.head
      val combinations = Board.chooseCombinations(board, next)
      combinations.foldLeft(allSolutions)((solutions, next_list) => {
        // board is now updated with new combination
        val updatedBoard = Board.updateBoard(board, next_list, next.color)
        helper(solutions, updatedBoard, predefinedList.tail)
      })
    }

    val s = helper(Set[Board](), boardObj, boardObj.predefinedCells)
    System.err.println("SOLUTIONS!!!!!!!")
    s.foreach(Board.printBoard(_))
  }

}

object Board {
  def parseString(boardStr: String) = {
    val predefinedCells = for {
      x <- {
        boardStr.split("\n").zipWithIndex;
      };
      y <- {
        x._1.split(",").zipWithIndex;
      }
      if y._1.trim != "()"
    } yield {
      val cell_value = y._1.trim
      val (n, c) = (cell_value.split("@")(0), cell_value.split("@")(1))
      new PredefinedCell(n.toInt, c, x._2, y._2)


    }

    val rows = boardStr.split("\n").length
    val cols = boardStr.split("\n")(0).split(",").length
    val dummy = Vector.tabulate(rows, cols) {
      (r, j) => Option[Cell](null)
    }
    // update the board based on read values. Board is defined.
    val board = predefinedCells.foldLeft(dummy)((myB, nextCell) =>
      myB.updated(nextCell.row, myB(nextCell.row).updated(nextCell.col, Some(nextCell)))

    )

    new Board(board, predefinedCells)


  }


  /**
    *
    * Updates the board. This can, however, throw exceptions
    *
    * @param boardObj
    * @param coordinates
    * @param color
    * @return
    */

  def updateBoard(boardObj: Board, coordinates: List[(Int, Int)], color: String): Board = {
    val b = boardObj.board
    val updated_board = coordinates.foldLeft(b)((board, next_coord) => {
      val (row, col) = (next_coord._1, next_coord._2)
      board.updated(row, board(row).updated(col, Some(Cell(0, color, row, col))))
    })
    new Board(updated_board, boardObj.predefinedCells)

  }


  /**
    *
    * Returns a list of directional neighbours
    *
    * @param cell
    * @return
    *
    */

  def calcNeighbours(boardObj: Board, cell: PredefinedCell): List[List[(Int, Int)]] = {

    val board = boardObj.board

    val rows = boardObj.rows
    val cols = boardObj.cols

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
    println("**********************************")

    print("%20s".format(""))
    (0 until board.cols) foreach (x => print("%20s".format(x)))
    println()



    val data = board.board

    for {
      x <- 0 until board.rows;
      y <- 0 until board.cols
    } yield {
      if (y == 0) print("%-20s".format(x))
      (data(x)(y): Option[Cell]) match {
        case Some(c: Cell) => print("%20s".format(c))
        case None => print("%20s".format("''"))
      }

      if (y == board.cols - 1) println()
    }
    println("**********************************")
  }


  def chooseCombinations(boardObj: Board,

                         cell: PredefinedCell)

  : Set[List[(Int, Int)]] = {

    chooseCombinations(boardObj, Board.calcNeighbours(boardObj, cell), Nil, List[List[(Int, Int)]](), cell.value, cell.value).map(x => x.sorted).toSet

  }


  def chooseCombinations(
                          boardObj: Board,
                          neighbour_lists: List[List[(Int, Int)]],
                          local_accumulator: List[(Int, Int)],
                          result_accumulator: List[List[(Int, Int)]],
                          max_depth: Int,
                          remaining: Int): List[List[(Int, Int)]] = {

    val board = boardObj.board
    if (neighbour_lists.isEmpty) result_accumulator;
    else {
      neighbour_lists.zipWithIndex.foldLeft(result_accumulator)((accum, nextList) => {
        // add the data
        if (remaining == 0) {
          return accum :+ local_accumulator
        }

        val element_idx = nextList._2
        val firstListElement = nextList._1.head
        // cannot take the element because something it is already taken
        if (boardObj.isOccupied(firstListElement._1, firstListElement._2)) {
          return chooseCombinations(boardObj, neighbour_lists.take(element_idx) ++ neighbour_lists.drop(element_idx + 1), local_accumulator, accum, max_depth, remaining)
        }


        // can take the element
        // for each one of the lists, we can take 1 element
        val local_accum = {
          if (max_depth == remaining) List(firstListElement) else local_accumulator :+ firstListElement
        }
        val remainingElements = {
          // drop the list completely if empty
          if (nextList._1.size == 1) neighbour_lists.take(element_idx) ++ neighbour_lists.drop(element_idx + 1) else neighbour_lists.updated(element_idx, neighbour_lists(element_idx).drop(1))
        }

        chooseCombinations(boardObj, remainingElements, local_accum, accum, max_depth, remaining - 1)
      }
      )
    }
  }

}

