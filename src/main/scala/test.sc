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
//println(chooseCombinations(calcNeighbours(board, predefinedCells(4)),2))

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

def chooseCombinations(l: List[List[(Int,Int)]], remaining: Int) :List[(Int,Int)] = {

  // 2 possibilities





  // not sure about the base case yet
  if (l.isEmpty) Nil
  else {
    // chose a list of first elements
    for {
        x<-l;
        if x.nonEmpty
    } yield x(0)
  }

}













//
//
//val predefined = (board flatMap (x => x map( y => y match {
//  case p:Some[Cell]=>p
//  case None   => None
//})) filter( x => x.isDefined))
//
//
////
////  map (y => y match {
////  case x:Some[PredefinedCell] => x.get.calcNeigh()
////  case None =>
////
////}
////)
//
//
//def process(predicates: Array[Option[Cell]], myB: Array[Array[Option[Cell]]]) = {
//  for {
//    x:Option[Cell] <- predicates
//    (r,c) <- {
//      x.get match {
//        case (c:PredefinedCell) =>  c.calcNeigh()
//      }
//    }
//    if x.get.value>0
//  } yield {
//
//  }
//
//    //println(r,c)
//
//}
//
//
//process(predefined, board)


/**
  * update on (2:Brown:(0,1)) rows(0 .. 2) columns (0..3
  * Running update on (2:Red:(1,1)) rows(0 .. 3) columns (0..3
  * Running update on (3:Blue:(2,0)) rows(0 .. 3) columns (0..3
  * Running update on (2:Yellow:(2,3)) rows(0 .. 3) columns (1..3
  * Running update on (2:Green:(3,3)) rows(1 .. 3) columns (1..3
  */


/*

   val possibleValues: List[(Int, String)]   = options
   // remove the value from the list of options
   def minusValue(c:String) = new Cell(v, c, this.options filterNot (x=>x._2 != c))

 */

//
//
//val predefined = board flatMap (x => x map( y => y match {
//  case p:Some[Cell]=>p
//  case None   => None
//})) filter( x => x.isDefined)
//
//for {
//  x<-predefined
//} yield updateBoard(board, x.get)
////predefined(0)
//
//printBoard(board)


//board(3)(2).


//
//val b:Option[Cell] =  board(1)(1)
//b match {
//  case Some(_) => print( "S ")
//  case None    => print(" d")
//}
//


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