package answers

object Puzzle4 extends App {
  val drawsSource = io.Source.fromFile("C:\\Users\\Sebastian Brock\\workspace\\advent_of_code_2021\\src\\main\\resources\\Puzzle4Draws.csv")
  val boardsSource = io.Source.fromFile("C:\\Users\\Sebastian Brock\\workspace\\advent_of_code_2021\\src\\main\\resources\\Puzzle4Boards.csv")

  val draws = drawsSource.getLines.drop(1).toList.map(_.toInt)
  val boardsUnparsed = boardsSource.getLines.drop(1).toList

  def getBoards(ub: List[String], b: List[List[String]]) : List[List[String]] = {
    val (i,k) = ub.span(_.nonEmpty)
    if(k.exists(_.isEmpty)) getBoards(k.drop(1), b:+ i) else b:+ i
  }

  val boards = getBoards(boardsUnparsed,Nil).map(_.map(_.split(' ').toList.filterNot(_.isEmpty)))

  /* Check number against boards
   * Mark spot with X
   * Check for Bingo
   * Take winning number multiply by unmarked numbers in that set
   * */

  // There's a better way to do this
  def checkRowForDraw(row: List[String], draw: String): List[String] = {
    row.find(r => r == draw) match {
      case Some(_) =>
        row.map(n => if(n == draw) "X" else n)
      case _ =>
        row
    }
  }

  def turnBoard(board: List[List[String]]) = {
    val y = for { i <- board.head.indices }
      yield {
        board.foldLeft(List(""))((x,r) => x :+ r(i))
    }

    y.toList.map(_.drop(1))
  }

  def checkBingo(board: List[List[String]])  = {

    println(s"Checking board for win: [$board]")
    val horizontalWin = board.exists(row => !row.exists(_ != "X"))
    val verticalWin = turnBoard(board).exists(row => !row.exists(_ != "X"))

    if(horizontalWin || verticalWin) Some(board) else None
  }

  def playRound(draw: Int, boards: List[List[List[String]]]) = {
    val newBoards = for { board <- boards } yield {
      board.map(r=> checkRowForDraw(r,draw.toString))
    }

    newBoards
  }

  def playGameFirstWinning(i: Int, boards: List[List[List[String]]]) : (List[List[String]], Int) = {
    val newBoards = playRound(draws(i), boards)
    newBoards.map(checkBingo).find (x => x.nonEmpty) match {
      case Some(winner) =>
        (winner.get, draws(i))
      case _ =>
        playGameFirstWinning(i + 1, newBoards)
    }
  }

  def solvePart1 = {
    val (board,draw) = playGameFirstWinning(0,boards)

    val sum = board.flatten.filterNot(_ == "X").map(_.toInt).sum
    sum * draw
  }

  println(s"Part 1 answer is: [$solvePart1]")

  def playGameSecondWinning(i: Int, boards: List[List[List[String]]]) : (List[List[String]], Int) = {
    val newBoards = playRound(draws(i), boards)

    //If last board is left and won, don't remove it.
    val removedWinners = if(newBoards.length != 1 )newBoards.zip(newBoards.map(checkBingo(_).isDefined)).filterNot(p => p._2).map(_._1) else newBoards

    // If last board is left and still hasn't won, draw again.
    if(removedWinners.length == 1 && removedWinners.map(checkBingo).exists(x => x.nonEmpty)) {
      (removedWinners.head, draws(i))
    } else { playGameSecondWinning(i + 1, removedWinners) }
  }

  def solvePart2 = {
    val (board,draw) = playGameSecondWinning(0,boards)
    val sum = board.flatten.filterNot(_ == "X").map(_.toInt).sum
    sum * draw
  }

  println(s"Part 2 answer is: [$solvePart2]")

}
