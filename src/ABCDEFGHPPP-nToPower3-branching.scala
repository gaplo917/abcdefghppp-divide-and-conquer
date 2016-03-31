import scala.collection.GenSeq
import scala.collection.parallel.ParSeq

object ABCDEFGHPPP_nToPower3_Cache {
  val base = 22
  val baseAnd1 = base + 1

  val width = 4

  val possibleToTry = (0 until base)
    .toVector
    .filter(x => x != 1)

  def intToChar(x:Int) = {
    if( x > 9) (x - 10 + 'a'.toInt).toChar
    else (x - 1 + '1'.toInt).toChar
  }

  object BranchState extends Enumeration {
    type BranchState = Value
    val Initial,State_1, State_11,State_0,State_10 = Value
  }
  import BranchState._


  def leadingDigitPossibleSolution(possibleToTry: Vector[Int], result:Int, lastLending:Int) = {
    possiblePermuatationN(possibleToTry,3)
      .filter {
        case xs@Vector(a,c,g) =>
          val na = a - lastLending
          val e = result - g
          na > 1 && na > c && c > 1 && g > 1 && na - c + g == result && e != 1 &&  !xs.contains(e)
      }
      .map{
        case xs@Vector(a,c,g) =>
          Vector(a, c, result - g, g)
      }.toStream.par
  }

  def innerDigitPossibleSolution(possibleToTry: Vector[Int], result:Int, lending:Int, lastLending:Int) = {
    possiblePermuatationN(possibleToTry,3)
      .filter {
        case xs@Vector(b,d,h) =>
          val f = result - h
          f != 1 && f >= 0  && f < base && (b + lending * base) - lastLending - d + h == result && f < base && !xs.contains(f)
      }
      .map{
        case xs@Vector(b,d,h) =>
          Vector(b, d, result - h, h)
      }.toStream.par
  }


  val InnerDigitPossibleSolutionMap  =
    Map(
      (State_11,1,1) -> innerDigitPossibleSolution(possibleToTry,baseAnd1,1,1),
      (State_11,1,0) -> innerDigitPossibleSolution(possibleToTry,baseAnd1,1,0),
      (State_11,0,1) -> innerDigitPossibleSolution(possibleToTry,baseAnd1,0,1),
      (State_11,0,0) -> innerDigitPossibleSolution(possibleToTry,baseAnd1,0,0),
      (State_10,1,1) -> innerDigitPossibleSolution(possibleToTry,base,1,1),
      (State_10,1,0) -> innerDigitPossibleSolution(possibleToTry,base,1,0),
      (State_10,0,1) -> innerDigitPossibleSolution(possibleToTry,base,0,1),
      (State_10,0,0) -> innerDigitPossibleSolution(possibleToTry,base,0,0)
    )


  val leadingDigitPossibleSolutionMap =
    Map(
      (State_11,1) -> leadingDigitPossibleSolution(possibleToTry,baseAnd1,1),
      (State_11,0) -> leadingDigitPossibleSolution(possibleToTry,baseAnd1,0),
      (State_10,1) -> leadingDigitPossibleSolution(possibleToTry,base,1),
      (State_10,0) -> leadingDigitPossibleSolution(possibleToTry,base,0)
    )

  def mainRecur() = {
    val a = func(
      possibleToTry = possibleToTry,
      branchState = State_11,
      landing = 1
    )

    println(s"finish a")

    val c = func(
      possibleToTry = possibleToTry,
      branchState = State_11,
      landing = 0
    )

    a ++ c
  }

  def func(possibleToTry: Vector[Int], branchState: BranchState = Initial,pos: Int = 1, landing:Int = 0, lastLending: Int = 0 ,possibleSolutions: ParSeq[Vector[Int]] = ParSeq()):  ParSeq[Vector[Int]] = {

    if(pos == width) {
      println("arrive end rec")
      val leadingDigitPossibleSolutions = leadingDigitPossibleSolutionMap(branchState,lastLending)

      if(pos > 1) leadingDigitPossibleSolutions.flatMap {
        case ps => possibleSolutions.filter(xs => !xs.contains(ps(0)) && !xs.contains(ps(1)) && !xs.contains(ps(2)) && !xs.contains(ps(3)) ).map(_ ++ ps)
      } else possibleSolutions

    } else {
      branchState match {
        case State_1 | State_0 =>
          // due to the fact that 1 - h > 0
          ParSeq()

        case state@ State_11 =>
          val innerDigitPossibleSolutions = InnerDigitPossibleSolutionMap(state,landing,lastLending)

          val newPossibleSolutions = if(pos > 1) innerDigitPossibleSolutions.flatMap {
            case ps => possibleSolutions.filter(xs => !xs.contains(ps(0)) && !xs.contains(ps(1)) && !xs.contains(ps(2)) && !xs.contains(ps(3)) ).map(_ ++ ps)
          } else innerDigitPossibleSolutions

          if(pos + 1 == width) {
            // no landing is allows if that is already the leading digit
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 0,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            )
          } else {
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 0,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            ) ++
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 1,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            )
          }

        case state@ State_10 =>
          val innerDigitPossibleSolutions = InnerDigitPossibleSolutionMap(state,landing,lastLending)

          val newPossibleSolutions = if(pos > 1) innerDigitPossibleSolutions.flatMap {
            case ps => possibleSolutions.filter(xs => !xs.contains(ps(0)) && !xs.contains(ps(1)) && !xs.contains(ps(2)) && !xs.contains(ps(3)) ).map(_ ++ ps)
          } else innerDigitPossibleSolutions

          if(pos + 1 == width) {
            // no landing is allows if that is already the leading digit
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 0,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            )
          } else {
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 0,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            ) ++
            func(
              possibleToTry = possibleToTry,
              branchState = State_10,
              landing = 1,
              lastLending = landing,
              pos = pos + 1,
              possibleSolutions = newPossibleSolutions
            )
          }

      }

    }

  }

  def possiblePermuatationN(xs: Vector[Int], n: Int) = {
    xs.combinations(n)
      .flatMap(_.permutations)
  }

  def main(args: Array[String]): Unit = {

    val start = System.currentTimeMillis()
    println("Start Calculation")

    val solutionsStream = mainRecur()

    val solutions = solutionsStream

    println(s"Total number of solutions = ${solutions}")

    println(s"Total Time used to solve AB - CD = 111 - GH = EF (Base $base): ${System.currentTimeMillis() - start}ms")

    if(solutions.size > 50 ) println(s"print out 50 solution :")

    solutions
      .take(50)
      .map(xs => xs.map(intToChar))
      .foreach {
      case xs =>
        // Hide printing log in benchmark
        println(s"$xs")
    }
  }
}

