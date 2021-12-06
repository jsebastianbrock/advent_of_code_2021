package answers

object Puzzle3 extends App {
  /*
   * Count most common bit from left to right.
   * Convert to decimal.
   * Convert inverse to decimal.
   * Multiply.
   * */
  val source = io.Source.fromFile("C:\\Users\\Sebastian Brock\\workspace\\advent_of_code_2021\\src\\main\\resources\\Puzzle3.csv")
  val data = source.getLines.drop(1).toList.map(_.toList.map(_.asDigit))

  def solvePart1(input: List[List[Int]]) : Int = {

    def flipBits(bits: List[Int]) = {
      bits.map(bit => if(bit == 0) 1 else 0)
    }

    def getDigit(index: Int) = {
      val (count0, count1) = input.foldLeft(0,0) { case ((countZero, countOne), entries) =>
        if(entries(index) == 1) (countZero, countOne + 1) else (countZero + 1, countOne)
      }
      if (count0 > count1) 0 else 1
    }

    val gammaBits = for {
      index <- input.head.indices
    } yield {
      getDigit(index)
    }

    val gamma = Integer.parseInt(gammaBits.mkString,2)
    val epsilon = Integer.parseInt(flipBits(gammaBits.toList).mkString, 2)
    println(s"gamma rate: [$gamma]")
    println(s"epsilon rate: [$epsilon]")
    gamma * epsilon
  }

  /*
   * For Oxygen
   * Count bits in first position
   * Keep bit entries that have most
   * Continue counting bits in position until left with one value.
   *
   * For C2O, do opposite
   *
   * Multiply.
   * */

  def solvePart2(data: List[List[Int]]) : Int = {
    def getOxygenDigit(input:List[List[Int]],index: Int) : Int = {
      val (count0, count1) = input.foldLeft(0,0) { case ((countZero, countOne), entries) =>
        if(entries(index) == 1) (countZero, countOne + 1) else (countZero + 1, countOne)
      }
      val digit = if (count1 >= count0) 1 else 0
      digit
    }

    def getCarbonDigit(input: List[List[Int]],index: Int) : Int = {
      val (count0, count1) = input.foldLeft(0,0) { case ((countZero, countOne), entries) =>
        if(entries(index) == 1) (countZero, countOne + 1) else (countZero + 1, countOne)
      }
      if (count1 < count0) 1 else 0
    }

    def findDiagnosticValue(input: List[List[Int]], getDigit: (List[List[Int]],Int) => Int) : List[List[Int]] = {
      def diagnosticSearch(index: Int, input: List[List[Int]]) : List[List[Int]] = {
        val highestForIndex = getDigit(input,index)
        val reducedInputs = input.filter(_(index) == highestForIndex)
        if (reducedInputs.length == 1) reducedInputs else diagnosticSearch(index + 1, reducedInputs)
      }
      diagnosticSearch(0,input)
    }

    val oxygenDiagnostic = findDiagnosticValue(data,getOxygenDigit).flatten.mkString
    println(s"Oxygen diagnostic: [$oxygenDiagnostic]")
    val carbonDiagnostic = findDiagnosticValue(data,getCarbonDigit).flatten.mkString
    println(s"Carbon diagnostic: [$carbonDiagnostic]")

    Integer.parseInt(oxygenDiagnostic,2) * Integer.parseInt(carbonDiagnostic,2)
  }

  println(s"Answer to part 1 is: [${solvePart1(data)}].")
  println(s"Answer to part 2 is: [${solvePart2(data)}].")
}
