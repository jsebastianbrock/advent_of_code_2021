package answers

object Puzzle4 extends App {
  val drawsSource = io.Source.fromFile("C:\\Users\\Sebastian Brock\\workspace\\advent_of_code_2021\\src\\main\\resources\\Puzzle4Draws.csv")
  val boardsSource = io.Source.fromFile("C:\\Users\\Sebastian Brock\\workspace\\advent_of_code_2021\\src\\main\\resources\\Puzzle4Boards.csv")

  val draws = drawsSource.getLines.drop(1).toList.map(_.toInt)
  println(s"Draws: $draws")

  val boardsUnparsed = boardsSource.getLines.drop(1).toList

  def getBoards(ub: List[String], b: List[List[String]]) : List[List[String]] = {
    val (i,k) = ub.span(_.nonEmpty)
    if(k.exists(_.isEmpty)) getBoards(k.drop(1), b:+ i) else b:+ i
  }

  val boards = getBoards(boardsUnparsed,Nil).map(_.map(_.split(' ').toList.filterNot(_.isEmpty)))

  println(s"Boards: $boards")

  /* Check number against boards
   * Mark spot with X
   * Check for Bingo
   * Take winning number multiply by unmarked numbers in that set
   * */

  // There's a better way to do this
  def checkRowForDraw(row: List[String], draw: String): List[String] = {
    row.find(r => r == draw) match {
      case Some(_) =>
        val newRow = row.map(n => if(n == draw) "X" else n)

        println(s"New row: [${newRow}]")
        newRow
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
    //((horizontalWin || verticalWin), board)
    println(s"Is win? Vert: [$verticalWin], horz: [$horizontalWin]")
    if(horizontalWin || verticalWin) Some(board) else None
  }

  def playRound(draw: Int, boards: List[List[List[String]]]) = {
    val newBoards = for { board <- boards } yield {
      board.map(r=> checkRowForDraw(r,draw.toString))
    }

    println(s"Draw: [$draw]")
    println(s"New Board: [${newBoards}]")
    newBoards
  }

  def playGame(i: Int, boards: List[List[List[String]]]) : (List[List[String]], Int) = {
    println(s"Draw count: [$i], Draw: [${draws(i)}]")
    val newBoards = playRound(draws(i), boards)
    newBoards.map(checkBingo).find (x => x.nonEmpty) match {
      case Some(winner) =>
        (winner.get, draws(i))
      case _ =>
        playGame(i + 1, newBoards)
    }

  }

  val (board,draw) = playGame(0,boards)

  println(s"winning draw: [$draw]")
  println(s"Winning board: [$board]")
  val sum = board.flatten.filterNot(_ == "X").map(_.toInt).sum
  val answer = sum * draw
  println(s"Answer is: [$answer]")
}
